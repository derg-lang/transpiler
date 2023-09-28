package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*

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
    SymbolType.AND to 4,
    SymbolType.DIVIDE to 0,
    SymbolType.EQUAL to 3,
    SymbolType.EXCLAMATION to 7,
    SymbolType.GREATER to 3,
    SymbolType.GREATER_EQUAL to 3,
    SymbolType.LESS to 3,
    SymbolType.LESS_EQUAL to 3,
    SymbolType.MINUS to 1,
    SymbolType.MODULO to 0,
    SymbolType.MULTIPLY to 0,
    SymbolType.NOT_EQUAL to 3,
    SymbolType.OR to 5,
    SymbolType.PLUS to 1,
    SymbolType.QUESTION to 7,
    SymbolType.THREE_WAY to 2,
    SymbolType.XOR to 6,
)

/**
 * Joins together the [lhs] and [rhs] expression using the specified [operator].
 */
private fun mergeInfix(lhs: AstExpression, operator: SymbolType, rhs: AstExpression): AstExpression = when (operator)
{
    SymbolType.AND           -> AstAnd(lhs, rhs)
    SymbolType.DIVIDE        -> AstDivide(lhs, rhs)
    SymbolType.EQUAL         -> AstEqual(lhs, rhs)
    SymbolType.EXCLAMATION   -> AstRaise(lhs, rhs)
    SymbolType.GREATER       -> AstGreater(lhs, rhs)
    SymbolType.GREATER_EQUAL -> AstGreaterEqual(lhs, rhs)
    SymbolType.LESS          -> AstLess(lhs, rhs)
    SymbolType.LESS_EQUAL    -> AstLessEqual(lhs, rhs)
    SymbolType.MINUS         -> AstSubtract(lhs, rhs)
    SymbolType.MODULO        -> AstModulo(lhs, rhs)
    SymbolType.MULTIPLY      -> AstMultiply(lhs, rhs)
    SymbolType.NOT_EQUAL     -> AstNotEqual(lhs, rhs)
    SymbolType.OR            -> AstOr(lhs, rhs)
    SymbolType.PLUS          -> AstAdd(lhs, rhs)
    SymbolType.QUESTION      -> AstCatch(lhs, rhs)
    SymbolType.THREE_WAY     -> AstThreeWay(lhs, rhs)
    SymbolType.XOR           -> AstXor(lhs, rhs)
    else                     -> throw IllegalStateException("Illegal operator $operator when parsing operator")
}

/**
 * Joins together the prefix [operator] and the [rhs] expression.
 */
private fun mergePrefix(operator: SymbolType, rhs: AstExpression): AstExpression = when (operator)
{
    SymbolType.NOT   -> AstNot(rhs)
    SymbolType.PLUS  -> AstPlus(rhs)
    SymbolType.MINUS -> AstMinus(rhs)
    else             -> throw IllegalStateException("Illegal operator $operator when parsing prefix operator")
}

/**
 * Joins together the [lhs] expression together with the remainder of the [terms], in a recursive manner.
 */
private fun mergeRecursively(lhs: AstExpression, terms: List<Pair<SymbolType, AstExpression>>, index: Int = 0): AstExpression
{
    val (op1, mhs) = terms.getOrNull(index) ?: return lhs
    val (op2, rhs) = terms.getOrNull(index + 1) ?: return mergeInfix(lhs, op1, mhs)
    
    // If next operator has higher precedence, parse left-hand of tree first
    if (PRECEDENCE[op1]!! <= PRECEDENCE[op2]!!)
        return mergeRecursively(mergeInfix(lhs, op1, mhs), terms, index + 1)
    
    // Otherwise, the remainder right-hand side must be parsed recursively
    val rest = mergeRecursively(rhs, terms, index + 2)
    return mergeInfix(lhs, op1, mergeInfix(mhs, op2, rest))
}

/**
 * Parses a single expression from the token stream.
 */
fun expressionParserOf(): Parser<AstExpression> =
    ParserPattern(::expressionPatternOf, ::expressionOutcomeOf)

private fun basePatternOf() = ParserAnyOf(
    ParserBool(),
    ParserReal(),
    ParserText(),
    variableCallParserOf(),
    functionCallParserOf(),
    subscriptCallParserOf(),
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

private fun expressionOutcomeOf(values: Parsers): AstExpression
{
    val base = values.get<AstExpression>("base")
    val terms = values.get<List<Parsers>>("terms")
    val ops = terms.produce<SymbolType>("operator")
    val rest = terms.produce<AstExpression>("term")
    return mergeRecursively(base, ops zip rest)
}

/**
 * Parses a variable access expression from the token stream.
 */
private fun variableCallParserOf(): Parser<AstExpression> =
    ParserPattern(::ParserName) { AstRead(it) }

/**
 * Parses a function call expression from the token stream.
 */
internal fun functionCallParserOf(): Parser<AstExpression> =
    ParserPattern(::functionCallPatternOf, ::functionCallOutcomeOf)

private fun functionCallPatternOf() = ParserSequence(
    "name" to ParserName(),
    "open" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
    "params" to ParserOptional(ParserRepeating(argumentParserOf(), ParserSymbol(SymbolType.COMMA))),
    "close" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
)

private fun functionCallOutcomeOf(outcome: Parsers): AstExpression =
    AstCall(outcome["name"], emptyList(), outcome["params"])

/**
 * Parses a subscript call expression from the token stream.
 */
private fun subscriptCallParserOf(): Parser<AstExpression> =
    ParserPattern(::subscriptCallPatternOf, ::subscriptCallOutcomeOf)

private fun subscriptCallPatternOf() = ParserSequence(
    "name" to ParserName(),
    "open" to ParserSymbol(SymbolType.OPEN_BRACKET),
    "params" to ParserOptional(ParserRepeating(argumentParserOf(), ParserSymbol(SymbolType.COMMA))),
    "close" to ParserSymbol(SymbolType.CLOSE_BRACKET),
)

private fun subscriptCallOutcomeOf(values: Parsers): AstExpression =
    AstSubscript(values["name"], values["params"])

/**
 * Parses an expression from in-between parenthesis from the token stream.
 */
private fun parenthesisParserOf(): Parser<AstExpression> =
    ParserPattern(::parenthesisPatternOf, ::parenthesisOutcomeOf)

private fun parenthesisPatternOf() = ParserSequence(
    "open" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
    "expr" to expressionParserOf(),
    "close" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
)

private fun parenthesisOutcomeOf(values: Parsers): AstExpression =
    values["expr"]

/**
 * Parses a unary operator from the token stream.
 */
private fun unaryOperatorParserOf(): Parser<AstExpression> =
    ParserPattern(::unaryOperatorPatternOf, ::unaryOperatorOutcomeOf)

private fun unaryOperatorPatternOf() = ParserSequence(
    "op" to ParserSymbol(SymbolType.PLUS, SymbolType.MINUS, SymbolType.NOT),
    "rhs" to basePatternOf(),
)

private fun unaryOperatorOutcomeOf(values: Parsers): AstExpression =
    mergePrefix(values["op"], values["rhs"])

/**
 * Parses a when expression from the token stream.
 */
private fun whenParserOf(): Parser<AstExpression> =
    ParserPattern(::whenPatternOf, ::whenOutcomeOf)

private fun whenPatternOf() = ParserSequence(
    "when" to ParserSymbol(SymbolType.WHEN),
    "expression" to expressionParserOf(),
    "first" to whenBranchParserOf(),
    "remainder" to ParserRepeating(whenBranchParserOf()),
    "else" to ParserOptional(ParserSequence("else" to ParserSymbol(SymbolType.ELSE), "expr" to expressionParserOf())),
)

private fun whenOutcomeOf(values: Parsers): AstExpression
{
    val expression = values.get<AstExpression>("expression")
    val default = values.get<Parsers?>("else")?.get<AstExpression>("expr")
    val first = listOf(values.get<Pair<AstExpression, AstExpression>>("first"))
    val branches = values.get<List<Pair<AstExpression, AstExpression>>>("remainder")
    return AstWhen(expression, first + branches, default)
}

/**
 * Parses a when expression branch from the token stream.
 */
private fun whenBranchParserOf(): Parser<Pair<AstExpression, AstExpression>> =
    ParserPattern(::whenBranchPatternOf, ::whenBranchOutcomeOf)

private fun whenBranchPatternOf() = ParserSequence(
    "condition" to expressionParserOf(),
    "separator" to ParserSymbol(SymbolType.ARROW),
    "expression" to expressionParserOf(),
)

private fun whenBranchOutcomeOf(values: Parsers): Pair<AstExpression, AstExpression>
{
    val cond = values.get<AstExpression>("condition")
    val expr = values.get<AstExpression>("expression")
    return cond to expr
}
