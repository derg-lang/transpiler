package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*

/**
 * Operators have a specific precedence associated with them. The higher the precedence, the later they are evaluated in
 * the expression. In other words, operators with the lowest precedence are evaluated first.
 *
 * Note that this table is not a complete set of all possible operators. Certain structural operators, such as
 * parenthesis, may be used to alter the precedence. The positioning of the operator also makes a difference - all
 * prefix operators (such as unary plus and minus) target only the expression to their immediate right, whereas postfix
 * operators target the entire expression so far to the left.
 */
private val PRECEDENCE = mapOf(
    Symbol.AND to 4,
    Symbol.DIVIDE to 0,
    Symbol.EQUAL to 3,
    Symbol.EXCLAMATION to 7,
    Symbol.GREATER to 3,
    Symbol.GREATER_EQUAL to 3,
    Symbol.LESS to 3,
    Symbol.LESS_EQUAL to 3,
    Symbol.MINUS to 1,
    Symbol.MODULO to 0,
    Symbol.MULTIPLY to 0,
    Symbol.NOT_EQUAL to 3,
    Symbol.OR to 5,
    Symbol.PLUS to 1,
    Symbol.QUESTION to 7,
    Symbol.THREE_WAY to 2,
    Symbol.XOR to 6,
)

/**
 * Joins together the [lhs] and [rhs] expression using the specified [operator].
 */
private fun mergeInfix(lhs: AstValue, operator: Symbol, rhs: AstValue): AstValue = when (operator)
{
    Symbol.AND           -> AstAnd(lhs, rhs)
    Symbol.DIVIDE        -> AstDivide(lhs, rhs)
    Symbol.EQUAL         -> AstEqual(lhs, rhs)
    Symbol.EXCLAMATION   -> AstRaise(lhs, rhs)
    Symbol.GREATER       -> AstGreater(lhs, rhs)
    Symbol.GREATER_EQUAL -> AstGreaterEqual(lhs, rhs)
    Symbol.LESS          -> AstLess(lhs, rhs)
    Symbol.LESS_EQUAL    -> AstLessEqual(lhs, rhs)
    Symbol.MINUS         -> AstSubtract(lhs, rhs)
    Symbol.MODULO        -> AstModulo(lhs, rhs)
    Symbol.MULTIPLY      -> AstMultiply(lhs, rhs)
    Symbol.NOT_EQUAL     -> AstNotEqual(lhs, rhs)
    Symbol.OR            -> AstOr(lhs, rhs)
    Symbol.PLUS          -> AstAdd(lhs, rhs)
    Symbol.QUESTION      -> AstCatch(lhs, rhs)
    Symbol.THREE_WAY     -> AstThreeWay(lhs, rhs)
    Symbol.XOR           -> AstXor(lhs, rhs)
    else                 -> throw IllegalStateException("Illegal operator $operator when parsing operator")
}

/**
 * Joins together the prefix [operator] and the [rhs] expression.
 */
private fun mergePrefix(operator: Symbol, rhs: AstValue): AstValue = when (operator)
{
    Symbol.NOT   -> AstNot(rhs)
    Symbol.PLUS  -> AstPlus(rhs)
    Symbol.MINUS -> AstMinus(rhs)
    else         -> throw IllegalStateException("Illegal operator $operator when parsing prefix operator")
}

/**
 * Joins together the [lhs] expression together with the remainder of the [terms], in a recursive manner.
 */
private fun mergeTerms(lhs: AstValue, terms: List<Pair<Symbol, AstValue>>, index: Int = 0): AstValue
{
    val (op1, mhs) = terms.getOrNull(index) ?: return lhs
    val (op2, rhs) = terms.getOrNull(index + 1) ?: return mergeInfix(lhs, op1, mhs)
    
    // If next operator has higher precedence, parse left-hand of tree first
    if (PRECEDENCE[op1]!! <= PRECEDENCE[op2]!!)
        return mergeTerms(mergeInfix(lhs, op1, mhs), terms, index + 1)
    
    // Otherwise, the remainder right-hand side must be parsed recursively
    val rest = mergeTerms(mergeInfix(mhs, op2, rhs), terms, index + 2)
    return mergeInfix(lhs, op1, rest)
}

/**
 * Parses a single expression from the token stream.
 */
fun expressionParserOf(): Parser<AstValue> =
    ParserPattern(::expressionPatternOf, ::expressionOutcomeOf)

private fun basePatternOf() = ParserAnyOf(
    ParserBool(),
    ParserInteger(),
    ParserText(),
    variableCallParserOf(),
    functionCallParserOf(),
    parenthesisParserOf(),
    unaryOperatorParserOf(),
    whenParserOf(),
)

private fun termPatternOf() = ParserSequence(
    "operator" to ParserSymbol(*PRECEDENCE.keys.toTypedArray()), // Note: This handles all infix operators
    "term" to basePatternOf(),
)

private fun expressionPatternOf() = ParserSequence(
    "base" to basePatternOf(),
    "terms" to ParserRepeating(termPatternOf()),
)

private fun expressionOutcomeOf(values: Parsers): AstValue
{
    val base = values.get<AstValue>("base")
    val terms = values.get<List<Parsers>>("terms")
    val ops = terms.produce<Symbol>("operator")
    val rest = terms.produce<AstValue>("term")
    return mergeTerms(base, ops zip rest)
}

/**
 * Parses a variable access expression from the token stream.
 */
private fun variableCallParserOf(): Parser<AstValue> =
    ParserPattern(::ParserName) { AstRead(it) }

/**
 * Parses a function call expression from the token stream.
 */
internal fun functionCallParserOf(): Parser<AstValue> =
    ParserPattern(::functionCallPatternOf, ::functionCallOutcomeOf)

private fun functionCallPatternOf() = ParserSequence(
    "name" to ParserName(),
    "open" to ParserSymbol(Symbol.OPEN_PARENTHESIS),
    "params" to ParserOptional(ParserRepeating(argumentParserOf(), ParserSymbol(Symbol.COMMA))),
    "close" to ParserSymbol(Symbol.CLOSE_PARENTHESIS),
)

private fun functionCallOutcomeOf(outcome: Parsers): AstValue =
    AstCall(outcome["name"], emptyList(), outcome["params"])

/**
 * Parses an expression from in-between parenthesis from the token stream.
 */
private fun parenthesisParserOf(): Parser<AstValue> =
    ParserPattern(::parenthesisPatternOf, ::parenthesisOutcomeOf)

private fun parenthesisPatternOf() = ParserSequence(
    "open" to ParserSymbol(Symbol.OPEN_PARENTHESIS),
    "expr" to expressionParserOf(),
    "close" to ParserSymbol(Symbol.CLOSE_PARENTHESIS),
)

private fun parenthesisOutcomeOf(values: Parsers): AstValue =
    values["expr"]

/**
 * Parses a unary operator from the token stream.
 */
private fun unaryOperatorParserOf(): Parser<AstValue> =
    ParserPattern(::unaryOperatorPatternOf, ::unaryOperatorOutcomeOf)

private fun unaryOperatorPatternOf() = ParserSequence(
    "op" to ParserSymbol(Symbol.PLUS, Symbol.MINUS, Symbol.NOT),
    "rhs" to basePatternOf(),
)

private fun unaryOperatorOutcomeOf(values: Parsers): AstValue =
    mergePrefix(values["op"], values["rhs"])

/**
 * Parses a when expression from the token stream.
 */
private fun whenParserOf(): Parser<AstValue> =
    ParserPattern(::whenPatternOf, ::whenOutcomeOf)

private fun whenPatternOf() = ParserSequence(
    "when" to ParserSymbol(Symbol.WHEN),
    "expression" to expressionParserOf(),
    "first" to whenBranchParserOf(),
    "remainder" to ParserRepeating(whenBranchParserOf()),
    "else" to ParserOptional(ParserSequence("else" to ParserSymbol(Symbol.ELSE), "expr" to expressionParserOf())),
)

private fun whenOutcomeOf(values: Parsers): AstValue
{
    val expression = values.get<AstValue>("expression")
    val default = values.get<Parsers?>("else")?.get<AstValue>("expr")
    val first = listOf(values.get<Pair<AstValue, AstValue>>("first"))
    val branches = values.get<List<Pair<AstValue, AstValue>>>("remainder")
    return AstWhen(expression, first + branches, default)
}

/**
 * Parses a when expression branch from the token stream.
 */
private fun whenBranchParserOf(): Parser<Pair<AstValue, AstValue>> =
    ParserPattern(::whenBranchPatternOf, ::whenBranchOutcomeOf)

private fun whenBranchPatternOf() = ParserSequence(
    "condition" to expressionParserOf(),
    "separator" to ParserSymbol(Symbol.ARROW),
    "expression" to expressionParserOf(),
)

private fun whenBranchOutcomeOf(values: Parsers): Pair<AstValue, AstValue>
{
    val cond = values.get<AstValue>("condition")
    val expr = values.get<AstValue>("expression")
    return cond to expr
}
