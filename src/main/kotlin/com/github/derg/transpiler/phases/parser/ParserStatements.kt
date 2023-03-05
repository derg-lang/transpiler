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
    SymbolType.ASSIGN_PLUS     -> Assignment.AssignAdd(name, rhs)
    SymbolType.ASSIGN_MINUS    -> Assignment.AssignSubtract(name, rhs)
    SymbolType.ASSIGN_MULTIPLY -> Assignment.AssignMultiply(name, rhs)
    SymbolType.ASSIGN_MODULO   -> Assignment.AssignModulo(name, rhs)
    SymbolType.ASSIGN_DIVIDE   -> Assignment.AssignDivide(name, rhs)
    else                       -> throw IllegalStateException("Illegal operator $operator when parsing assignment")
}

/**
 * Parses a single statement from the token stream.
 */
fun statementParserOf(): Parser<Statement> =
    ParserPattern(::statementPatternOf) { it }

private fun statementPatternOf(): Parser<Statement> = ParserAnyOf(
    variableParserOf(),
    functionParserOf(),
    typeParserOf(),
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

private fun assignmentOutcomeOf(values: Parsers): Assignment?
{
    val name = values.produce<Name>("name") ?: return null
    val op = values.produce<SymbolType>("op") ?: return null
    val rhs = values.produce<Expression>("rhs") ?: return null
    return merge(name, op, rhs)
}

/**
 * Parses a single branch control from the token stream.
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
 * Parses a single raise control flow from the token stream.
 */
private fun raiseParserOf(): Parser<Statement> =
    ParserPattern(::raisePatternOf, ::raiseOutcomeOf)

private fun raisePatternOf() = ParserSequence(
    "symbol" to ParserSymbol(SymbolType.RAISE),
    "expression" to expressionParserOf(),
)

private fun raiseOutcomeOf(values: Parsers): Control.Raise?
{
    val expression = values.produce<Expression>("expression") ?: return null
    return Control.Raise(expression)
}

/**
 * Parses a single return control flow from the token stream.
 */
private fun returnParserOf(): Parser<Statement> =
    ParserPattern(::returnPatternOf, ::returnOutcomeOf)

private fun returnPatternOf() = ParserSequence(
    "symbol" to ParserSymbol(SymbolType.RETURN),
    "expression" to expressionParserOf(),
)

private fun returnOutcomeOf(values: Parsers): Control.Return?
{
    val expression = values.produce<Expression>("expression") ?: return null
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

/**
 * Parses a variable definition from the token stream.
 */
fun variableParserOf(): Parser<Statement> =
    ParserPattern(::variablePatternOf, ::variableOutcomeOf)

private fun variablePatternOf() = ParserSequence(
    "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
    "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
    "name" to ParserName(),
    "op" to ParserSymbol(SymbolType.ASSIGN),
    "value" to expressionParserOf(),
)

private fun variableOutcomeOf(values: Parsers): Definition?
{
    return Definition.Variable(
        name = values.produce("name") ?: return null,
        type = null,
        value = values.produce("value") ?: return null,
        visibility = visibilityOf(values.produce("visibility")),
        mutability = mutabilityOf(values.produce("mutability") ?: return null),
    )
}

/**
 * Parses a function definition from the token stream.
 */
fun functionParserOf(): Parser<Statement> =
    ParserPattern(::functionPatternOf, ::functionOutcomeOf)

private fun functionPatternOf() = ParserSequence(
    "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
    "fun" to ParserSymbol(SymbolType.FUN),
    "name" to ParserName(),
    "open_parenthesis" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
    "parameters" to ParserRepeating(parameterParserOf(), ParserSymbol(SymbolType.COMMA)),
    "close_parenthesis" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
    "error" to ParserOptional(nameParserOf(SymbolType.COLON)),
    "value" to ParserOptional(nameParserOf(SymbolType.ARROW)),
    "open_brace" to ParserSymbol(SymbolType.OPEN_BRACE),
    "statements" to ParserRepeating(statementParserOf()),
    "close_brace" to ParserSymbol(SymbolType.CLOSE_BRACE),
)

private fun functionOutcomeOf(values: Parsers): Definition?
{
    return Definition.Function(
        name = values.produce("name") ?: return null,
        valueType = values.produce("value"),
        errorType = values.produce("error"),
        parameters = values.produce("parameters") ?: return null,
        visibility = visibilityOf(values.produce("visibility")),
        statements = values.produce("statements") ?: return null,
    )
}

/**
 * Parses a type definition from the token stream.
 */
fun typeParserOf(): Parser<Statement> =
    ParserPattern(::typePatternOf, ::typeOutcomeOf)

private fun typePatternOf() = ParserSequence(
    "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
    "type" to ParserSymbol(SymbolType.TYPE),
    "name" to ParserName(),
    "open_brace" to ParserSymbol(SymbolType.OPEN_BRACE),
    "properties" to ParserRepeating(propertyParserOf()),
    "close_brace" to ParserSymbol(SymbolType.CLOSE_BRACE),
)

private fun typeOutcomeOf(values: Parsers): Definition?
{
    return Definition.Type(
        name = values.produce("name") ?: return null,
        visibility = visibilityOf(values.produce("visibility")),
        properties = values.produce("properties") ?: emptyList(),
    )
}
