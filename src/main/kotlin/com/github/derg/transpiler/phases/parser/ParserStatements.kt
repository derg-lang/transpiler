package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.SymbolType

/**
 * Joins together the [name] with the [operator] and the [rhs] expression.
 */
private fun merge(name: Name, operator: SymbolType, rhs: Expression): Assignment = when (operator)
{
    SymbolType.ASSIGN          -> Assignment.Assign(name, rhs)
    SymbolType.ASSIGN_PLUS     -> Assignment.AssignAdd(name, rhs)
    SymbolType.ASSIGN_MINUS    -> Assignment.AssignSubtract(name, rhs)
    SymbolType.ASSIGN_MULTIPLY -> Assignment.AssignMultiply(name, rhs)
    SymbolType.ASSIGN_MODULO   -> Assignment.AssignModulo(name, rhs)
    SymbolType.ASSIGN_DIVIDE   -> Assignment.AssignDivide(name, rhs)
    else                       -> throw IllegalStateException("Illegal operator $operator when parsing assignment")
}

/**
 * Generates a new fresh set of the base statements parsers. This generator parses recursively, where sub-parsers may
 * require parsing additional statements.
 */
private fun generateStandardParser(): Parser<Statement> = ParserAnyOf(
    variableParserOf(),
    functionParserOf(),
    assignmentParserOf(),
    branchParserOf(),
    callParserOf(),
    raiseParserOf(),
    returnParserOf(),
)

/**
 * Parses a single statement from the provided token.
 */
fun statementParserOf(): Parser<Statement> =
    ParserPattern(::generateStandardParser) { it }

/**
 * Parses a single scope from the provided token.
 */
fun scopeParserOf(): Parser<Scope> =
    ParserPattern(::scopePatternOf, ::scopeOutcomeOf)

private fun scopePatternOf() = ParserAnyOf(
    ParserSequence("single" to statementParserOf()),
    ParserSequence(
        "open" to ParserSymbol(SymbolType.OPEN_BRACE),
        "multiple" to ParserRepeating(statementParserOf()),
        "close" to ParserSymbol(SymbolType.CLOSE_BRACE),
    )
)

private fun scopeOutcomeOf(values: Parsers): Scope?
{
    val statement = values.produce<Statement>("single")
    val statements = values.produce<List<Statement>>("multiple")
    val isBraced = statement == null
    return Scope(isBraced, statements ?: statement?.let { listOf(it) } ?: return null)
}

/**
 * Parses a single assignment from the provided token.
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

private fun assignmentOutcomeOf(values: Parsers): Assignment?
{
    val name = values.produce<Name>("name") ?: return null
    val op = values.produce<SymbolType>("op") ?: return null
    val rhs = values.produce<Expression>("rhs") ?: return null
    return merge(name, op, rhs)
}

/**
 * Parses a single branch control from the provided token.
 */
private fun branchParserOf(): Parser<Statement> =
    ParserPattern(::branchPatternOf, ::branchOutcomeOf)

private fun branchPatternOf() = ParserSequence(
    "if" to ParserSymbol(SymbolType.IF),
    "predicate" to expressionParserOf(),
    "success" to scopeParserOf(),
    "other" to ParserOptional(ParserSequence("else" to ParserSymbol(SymbolType.ELSE), "failure" to scopeParserOf())),
)

private fun branchOutcomeOf(values: Parsers): Control.Branch?
{
    val predicate = values.produce<Expression>("predicate") ?: return null
    val success = values.produce<Scope>("success") ?: return null
    val failure = values.produce<Parsers>("other")?.produce<Scope>("failure")
    return Control.Branch(predicate, success, failure)
}

/**
 * Parses a single raise control flow from the provided token.
 */
private fun raiseParserOf(): Parser<Statement> =
    ParserPattern(::raisePatternOf, ::raiseOutcomeOf)

private fun raisePatternOf() = ParserSequence(
    "symbol" to ParserSymbol(SymbolType.RETURN_ERROR),
    "expression" to expressionParserOf(),
)

private fun raiseOutcomeOf(values: Parsers): Control.Raise?
{
    val expression = values.produce<Expression>("expression") ?: return null
    return Control.Raise(expression)
}

/**
 * Parses a single return control flow from the provided token.
 */
private fun returnParserOf(): Parser<Statement> =
    ParserPattern(::returnPatternOf, ::returnOutcomeOf)

private fun returnPatternOf() = ParserSequence(
    "symbol" to ParserSymbol(SymbolType.RETURN_VALUE),
    "expression" to expressionParserOf(),
)

private fun returnOutcomeOf(values: Parsers): Control.Return?
{
    val expression = values.produce<Expression>("expression") ?: return null
    // TODO: The magic string `_` should not be used. Use the type-information of the function to remove it
    return if (expression == Access.Variable("_")) Control.Return(null) else Control.Return(expression)
}

/**
 * Parses a single expression statement from the provided token. Note that the parser does not implement analysis to
 * determine whether the expression has any value or error types.
 */
// TODO: Not a correct implementation of the parser - must also function with error handling
private fun callParserOf(): Parser<Statement> =
    ParserPattern(::functionCallParserOf) { Control.Call(it) }
