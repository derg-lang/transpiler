package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*

/**
 * Joins together the [name] with the [operator] and the [rhs] expression.
 */
private fun merge(name: String, operator: Symbol, rhs: AstValue): AstInstruction = when (operator)
{
    Symbol.ASSIGN          -> AstAssign(name, rhs)
    Symbol.ASSIGN_PLUS     -> AstAssignAdd(name, rhs)
    Symbol.ASSIGN_MINUS    -> AstAssignSubtract(name, rhs)
    Symbol.ASSIGN_MULTIPLY -> AstAssignMultiply(name, rhs)
    Symbol.ASSIGN_MODULO   -> AstAssignModulo(name, rhs)
    Symbol.ASSIGN_DIVIDE   -> AstAssignDivide(name, rhs)
    else                   -> throw IllegalStateException("Illegal operator $operator when parsing assignment")
}

/**
 * Parses a single statement from the token stream.
 */
fun statementParserOf(): Parser<AstInstruction> = ParserAnyOf(
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
private fun assignmentParserOf(): Parser<AstInstruction> =
    ParserPattern(::assignmentPatternOf, ::assignmentOutcomeOf)

private fun assignmentPatternOf() = ParserSequence(
    "name" to ParserName(),
    "op" to ParserSymbol(
        Symbol.ASSIGN,
        Symbol.ASSIGN_PLUS,
        Symbol.ASSIGN_MINUS,
        Symbol.ASSIGN_MULTIPLY,
        Symbol.ASSIGN_DIVIDE,
        Symbol.ASSIGN_MODULO,
    ),
    "rhs" to expressionParserOf(),
)

private fun assignmentOutcomeOf(values: Parsers): AstInstruction =
    merge(values["name"], values["op"], values["rhs"])

/**
 * Parses a single branch control from the token stream.
 */
private fun branchParserOf(): Parser<AstInstruction> =
    ParserPattern(::branchPatternOf, ::branchOutcomeOf)

private fun branchPatternOf() = ParserSequence(
    "if" to ParserSymbol(Symbol.IF),
    "predicate" to expressionParserOf(),
    "success" to scopeParserOf(),
    "failure" to ParserOptional(ParserPattern(::branchElsePatternOf, ::branchElseOutcomeOf), emptyList()),
)

private fun branchOutcomeOf(values: Parsers): AstBranch =
    AstBranch(values["predicate"], values["success"], values["failure"])

private fun branchElsePatternOf() =
    ParserSequence("symbol" to ParserSymbol(Symbol.ELSE), "scope" to scopeParserOf())

private fun branchElseOutcomeOf(values: Parsers): List<AstInstruction> =
    values["scope"]

/**
 * Parses a single raise control flow from the token stream.
 */
private fun raiseParserOf(): Parser<AstInstruction> =
    ParserPattern(::raisePatternOf, ::raiseOutcomeOf)

private fun raisePatternOf() = ParserSequence(
    "symbol" to ParserSymbol(Symbol.RAISE),
    "expression" to expressionParserOf(),
)

private fun raiseOutcomeOf(values: Parsers): AstReturnError =
    AstReturnError(values["expression"])

/**
 * Parses a single return control flow from the token stream.
 */
private fun returnParserOf(): Parser<AstInstruction> =
    ParserPattern(::returnPatternOf, ::returnOutcomeOf)

private fun returnPatternOf() = ParserSequence(
    "symbol" to ParserSymbol(Symbol.RETURN),
    "expression" to expressionParserOf(),
)

private fun returnOutcomeOf(values: Parsers): AstInstruction
{
    val expression = values.get<AstValue>("expression")
    // TODO: The magic string `_` should not be used. Use the type-information of the function to remove it
    return if (expression == AstLoad("_", emptyList())) AstReturn else AstReturnValue(expression)
}

/**
 * Parses a single expression statement from the token stream. Note that the parser does not implement analysis to
 * determine whether the expression has any value or error types.
 */
// TODO: Not a correct implementation of the parser - must also function with error handling
private fun invokeParserOf(): Parser<AstInstruction> =
    ParserPattern(::callParserOf) { AstEvaluate(it) }
