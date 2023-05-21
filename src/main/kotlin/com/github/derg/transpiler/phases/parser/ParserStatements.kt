package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.SymbolType

/**
 * Joins together the [name] with the [operator] and the [rhs] expression.
 */
private fun merge(name: Name, operator: SymbolType, rhs: Expression): Assignment = when (operator)
{
    SymbolType.ASSIGN          -> Assignment.Assign(name, rhs)
    SymbolType.ASSIGN_PLUS     -> Assignment.Assign(name, Operator.Add(Access.Variable(name), rhs))
    SymbolType.ASSIGN_MINUS    -> Assignment.Assign(name, Operator.Subtract(Access.Variable(name), rhs))
    SymbolType.ASSIGN_MULTIPLY -> Assignment.Assign(name, Operator.Multiply(Access.Variable(name), rhs))
    SymbolType.ASSIGN_MODULO   -> Assignment.Assign(name, Operator.Modulo(Access.Variable(name), rhs))
    SymbolType.ASSIGN_DIVIDE   -> Assignment.Assign(name, Operator.Divide(Access.Variable(name), rhs))
    else                       -> throw IllegalStateException("Illegal operator $operator when parsing assignment")
}

/**
 * Parses a single statement from the token stream.
 */
fun statementParserOf(): Parser<Statement> = ParserAnyOf(
    variableParserOf(),
    assignmentParserOf(),
    branchParserOf(),
    invokeParserOf(),
    raiseParserOf(),
    returnParserOf(),
)

/**
 * Parses a single assignment from the token stream.
 */
private fun assignmentParserOf(): Parser<Statement> =
    ParserPattern(::assignmentPatternOf, ::assignmentOutcomeOf)

private fun assignmentPatternOf() = ParserSequence(
    "name" to ParserName(),
    "op" to ParserSymbol(
        SymbolType.ASSIGN,
        SymbolType.ASSIGN_PLUS,
        SymbolType.ASSIGN_MINUS,
        SymbolType.ASSIGN_MULTIPLY,
        SymbolType.ASSIGN_DIVIDE,
        SymbolType.ASSIGN_MODULO,
    ),
    "rhs" to expressionParserOf(),
)

private fun assignmentOutcomeOf(values: Parsers): Assignment =
    merge(values["name"], values["op"], values["rhs"])

/**
 * Parses a single branch control from the token stream.
 */
private fun branchParserOf(): Parser<Statement> =
    ParserPattern(::branchPatternOf, ::branchOutcomeOf)

private fun branchPatternOf() = ParserSequence(
    "if" to ParserSymbol(SymbolType.IF),
    "predicate" to expressionParserOf(),
    "success" to scopeParserOf(),
    "failure" to ParserOptional(ParserPattern(::branchElsePatternOf, ::branchElseOutcomeOf), emptyList()),
)

private fun branchOutcomeOf(values: Parsers): Control.Branch =
    Control.Branch(values["predicate"], values["success"], values["failure"])

private fun branchElsePatternOf() =
    ParserSequence("symbol" to ParserSymbol(SymbolType.ELSE), "scope" to scopeParserOf())

private fun branchElseOutcomeOf(values: Parsers): List<Statement> =
    values["scope"]

/**
 * Parses a single raise control flow from the token stream.
 */
private fun raiseParserOf(): Parser<Statement> =
    ParserPattern(::raisePatternOf, ::raiseOutcomeOf)

private fun raisePatternOf() = ParserSequence(
    "symbol" to ParserSymbol(SymbolType.RAISE),
    "expression" to expressionParserOf(),
)

private fun raiseOutcomeOf(values: Parsers): Control.Raise =
    Control.Raise(values["expression"])

/**
 * Parses a single return control flow from the token stream.
 */
private fun returnParserOf(): Parser<Statement> =
    ParserPattern(::returnPatternOf, ::returnOutcomeOf)

private fun returnPatternOf() = ParserSequence(
    "symbol" to ParserSymbol(SymbolType.RETURN),
    "expression" to expressionParserOf(),
)

private fun returnOutcomeOf(values: Parsers): Control.Return
{
    val expression = values.get<Expression>("expression")
    // TODO: The magic string `_` should not be used. Use the type-information of the function to remove it
    return if (expression == Access.Variable("_")) Control.Return(null) else Control.Return(expression)
}

/**
 * Parses a single expression statement from the token stream. Note that the parser does not implement analysis to
 * determine whether the expression has any value or error types.
 */
// TODO: Not a correct implementation of the parser - must also function with error handling
private fun invokeParserOf(): Parser<Statement> =
    ParserPattern(::functionCallParserOf) { Control.Invoke(it) }
