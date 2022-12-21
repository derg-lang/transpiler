package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.SymbolType

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
    SymbolType.CATCH to 7,
    SymbolType.DIVIDE to 0,
    SymbolType.EQUAL to 3,
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
    SymbolType.RAISE to 7,
    SymbolType.THREE_WAY to 2,
    SymbolType.XOR to 6,
)

/**
 * Joins together the [lhs] and [rhs] expression using the specified [operator].
 */
private fun mergeInfix(lhs: Expression, operator: SymbolType, rhs: Expression): Expression = when (operator)
{
    SymbolType.AND           -> Operator.And(lhs, rhs)
    SymbolType.CATCH         -> Operator.Catch(lhs, rhs)
    SymbolType.DIVIDE        -> Operator.Divide(lhs, rhs)
    SymbolType.EQUAL         -> Operator.Equal(lhs, rhs)
    SymbolType.GREATER       -> Operator.Greater(lhs, rhs)
    SymbolType.GREATER_EQUAL -> Operator.GreaterEqual(lhs, rhs)
    SymbolType.LESS          -> Operator.Less(lhs, rhs)
    SymbolType.LESS_EQUAL    -> Operator.LessEqual(lhs, rhs)
    SymbolType.MINUS         -> Operator.Subtract(lhs, rhs)
    SymbolType.MODULO        -> Operator.Modulo(lhs, rhs)
    SymbolType.MULTIPLY      -> Operator.Multiply(lhs, rhs)
    SymbolType.NOT_EQUAL     -> Operator.NotEqual(lhs, rhs)
    SymbolType.OR            -> Operator.Or(lhs, rhs)
    SymbolType.PLUS          -> Operator.Add(lhs, rhs)
    SymbolType.RAISE         -> Operator.Raise(lhs, rhs)
    SymbolType.THREE_WAY     -> Operator.ThreeWay(lhs, rhs)
    SymbolType.XOR           -> Operator.Xor(lhs, rhs)
    else                     -> throw IllegalStateException("Illegal operator $operator when parsing operator")
}

/**
 * Joins together the prefix [operator] and the [rhs] expression.
 */
private fun mergePrefix(operator: SymbolType, rhs: Expression): Expression = when (operator)
{
    SymbolType.NOT   -> Operator.Not(rhs)
    SymbolType.PLUS  -> Operator.Plus(rhs)
    SymbolType.MINUS -> Operator.Minus(rhs)
    else             -> throw IllegalStateException("Illegal operator $operator when parsing prefix operator")
}

/**
 * Joins together the [lhs] expression together with the remainder of the [terms], in a recursive manner.
 */
private fun mergeRecursively(lhs: Expression, terms: List<Pair<SymbolType, Expression>>, index: Int = 0): Expression
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
fun expressionParserOf(): Parser<Expression> =
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

private fun expressionOutcomeOf(values: Parsers): Expression?
{
    val base = values.produce<Expression>("base") ?: return null
    val terms = values.produce<List<Parsers>>("terms") ?: emptyList()
    val ops = terms.produce<SymbolType>("operator")
    val rest = terms.produce<Expression>("term")
    return mergeRecursively(base, ops.zip(rest))
}

/**
 * Parses a variable access expression from the token stream.
 */
private fun variableCallParserOf(): Parser<Expression> =
    ParserPattern(::ParserName) { Access.Variable(it) }

/**
 * Parses a function call expression from the token stream.
 */
internal fun functionCallParserOf(): Parser<Expression> =
    ParserPattern(::functionCallPatternOf, ::functionCallOutcomeOf)

private fun functionCallPatternOf() = ParserSequence(
    "name" to ParserName(),
    "open" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
    "params" to ParserOptional(ParserRepeating(parameterParserOf(), ParserSymbol(SymbolType.COMMA))),
    "close" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
)

private fun functionCallOutcomeOf(outcome: Parsers): Expression?
{
    val name = outcome.produce<Name>("name") ?: return null
    val params = outcome.produce<List<Parameter>>("params") ?: emptyList()
    return Access.Function(name, params)
}

/**
 * Parses a subscript call expression from the token stream.
 */
private fun subscriptCallParserOf(): Parser<Expression> =
    ParserPattern(::subscriptCallPatternOf, ::subscriptCallOutcomeOf)

private fun subscriptCallPatternOf() = ParserSequence(
    "name" to ParserName(),
    "open" to ParserSymbol(SymbolType.OPEN_BRACKET),
    "params" to ParserOptional(ParserRepeating(parameterParserOf(), ParserSymbol(SymbolType.COMMA))),
    "close" to ParserSymbol(SymbolType.CLOSE_BRACKET),
)

private fun subscriptCallOutcomeOf(values: Parsers): Expression?
{
    val name = values.produce<Name>("name") ?: return null
    val params = values.produce<List<Parameter>>("params") ?: emptyList()
    return Access.Subscript(name, params)
}

/**
 * Parses a function call parameter from the token stream.
 */
private fun parameterParserOf(): Parser<Parameter> =
    ParserPattern(::parameterPatternOf, ::parameterOutcomeOf)

private fun parameterPatternOf() = ParserAnyOf(
    ParserSequence("expr" to expressionParserOf()),
    ParserSequence("name" to ParserName(), "sym" to ParserSymbol(SymbolType.ASSIGN), "expr" to expressionParserOf()),
)

private fun parameterOutcomeOf(values: Parsers): Parameter?
{
    val name = values.produce<Name>("name")
    val expression = values.produce<Expression>("expr") ?: return null
    return Parameter(name, expression)
}

/**
 * Parses an expression from in-between parenthesis from the token stream.
 */
private fun parenthesisParserOf(): Parser<Expression> =
    ParserPattern(::parenthesisPatternOf, ::parenthesisOutcomeOf)

private fun parenthesisPatternOf() = ParserSequence(
    "open" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
    "expr" to expressionParserOf(),
    "close" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
)

private fun parenthesisOutcomeOf(values: Parsers): Expression? =
    values.produce("expr")

/**
 * Parses a unary operator from the token stream.
 */
private fun unaryOperatorParserOf(): Parser<Expression> =
    ParserPattern(::unaryOperatorPatternOf, ::unaryOperatorOutcomeOf)

private fun unaryOperatorPatternOf() = ParserSequence(
    "op" to ParserSymbol(SymbolType.PLUS, SymbolType.MINUS, SymbolType.NOT),
    "rhs" to basePatternOf(),
)

private fun unaryOperatorOutcomeOf(values: Parsers): Expression?
{
    val op = values.produce<SymbolType>("op") ?: return null
    val rhs = values.produce<Expression>("rhs") ?: return null
    return mergePrefix(op, rhs)
}

/**
 * Parses a when expression from the token stream.
 */
private fun whenParserOf(): Parser<Expression> =
    ParserPattern(::whenPatternOf, ::whenOutcomeOf)

private fun whenPatternOf() = ParserSequence(
    "when" to ParserSymbol(SymbolType.WHEN),
    "expression" to expressionParserOf(),
    "first" to whenBranchParserOf(),
    "remainder" to ParserRepeating(whenBranchParserOf()),
    "else" to ParserOptional(ParserSequence("else" to ParserSymbol(SymbolType.ELSE), "expr" to expressionParserOf())),
)

private fun whenOutcomeOf(values: Parsers): Expression?
{
    val expression = values.produce<Expression>("expression") ?: return null
    val default = values.produce<Parsers>("else")?.produce<Expression>("expr")
    val first = listOf(values.produce<Pair<Expression, Expression>>("first") ?: return null)
    val branches = values.produce<List<Pair<Expression, Expression>>>("remainder") ?: return null
    return When(expression, first + branches, default)
}

/**
 * Parses a when expression branch from the token stream.
 */
private fun whenBranchParserOf(): Parser<Pair<Expression, Expression>> =
    ParserPattern(::whenBranchPatternOf, ::whenBranchOutcomeOf)

private fun whenBranchPatternOf() = ParserSequence(
    "condition" to expressionParserOf(),
    "separator" to ParserSymbol(SymbolType.ARROW),
    "expression" to expressionParserOf(),
)

private fun whenBranchOutcomeOf(values: Parsers): Pair<Expression, Expression>?
{
    val cond = values.produce<Expression>("condition") ?: return null
    val expr = values.produce<Expression>("expression") ?: return null
    return cond to expr
}
