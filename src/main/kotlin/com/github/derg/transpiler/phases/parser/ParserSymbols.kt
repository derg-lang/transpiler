package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*

/**
 * Parses a single statement from the token stream.
 */
fun definitionParserOf(): Parser<AstSymbol> = ParserAnyOf(
    constantParserOf(),
    functionParserOf(),
    structParserOf(),
)

/**
 * Parses a variable definition from the token stream.
 */
fun constantParserOf(): Parser<AstConstant> =
    ParserPattern(::constantPatternOf, ::constantOutcomeOf)

private fun constantPatternOf() = ParserSequence(
    "visibility" to visibilityParserOf(),
    "assignability" to ParserSymbol(Symbol.VALUE),
    "name" to ParserIdentifier(),
    "kind" to optionalKindParserOf(Symbol.COLON),
    "op" to ParserSymbol(Symbol.ASSIGN),
    "value" to ParserExpression(),
)

private fun constantOutcomeOf(values: Parsers) = AstConstant(
    name = values["name"],
    kind = values["kind"],
    value = values["value"],
    visibility = values["visibility"],
)

/**
 * Parses a function definition from the token stream.
 */
fun functionParserOf(): Parser<AstFunction> =
    ParserPattern(::functionPatternOf, ::functionOutcomeOf)

private fun functionPatternOf() = ParserSequence(
    "visibility" to visibilityParserOf(),
    "fun" to ParserSymbol(Symbol.FUN),
    "name" to ParserIdentifier(),
    "templates" to ParserOptional(typeParameterListParserOf()),
    "parameters" to parameterListParserOf(),
    "error" to optionalKindParserOf(Symbol.COLON),
    "value" to optionalKindParserOf(Symbol.ARROW),
    "open_brace" to ParserSymbol(Symbol.OPEN_BRACE),
    "statements" to ParserRepeating(statementParserOf()),
    "close_brace" to ParserSymbol(Symbol.CLOSE_BRACE),
)

private fun functionOutcomeOf(values: Parsers) = AstFunction(
    name = values["name"],
    valueKind = values["value"] ?: AstKind.Nothing,
    errorKind = values["error"] ?: AstKind.Nothing,
    typeParameters = values["templates"] ?: emptyList(),
    parameters = values["parameters"],
    visibility = values["visibility"],
    statements = values["statements"],
)

/**
 * Parses a struct definition from the token stream.
 */
fun structParserOf(): Parser<AstStruct> =
    ParserPattern(::structPatternOf, ::structOutcomeOf)

private fun structPatternOf() = ParserSequence(
    "visibility" to visibilityParserOf(),
    "struct" to ParserSymbol(Symbol.STRUCT),
    "name" to ParserIdentifier(),
    "type_params" to ParserOptional(typeParameterListParserOf()),
    "open_brace" to ParserSymbol(Symbol.OPEN_BRACE),
    "properties" to ParserRepeating(propertyParserOf()),
    "close_brace" to ParserSymbol(Symbol.CLOSE_BRACE),
)

private fun structOutcomeOf(values: Parsers) = AstStruct(
    name = values["name"],
    visibility = values["visibility"],
    fields = values["properties"],
    typeParameters = values["type_params"] ?: emptyList(),
)

/**
 * Parses a variable definition from the token stream.
 */
fun variableParserOf(): Parser<AstVariable> =
    ParserPattern(::variablePatternOf, ::variableOutcomeOf)

private fun variablePatternOf() = ParserSequence(
    "visibility" to visibilityParserOf(),
    "mutability" to mutabilityParserOf(),
    "assignability" to assignabilityParserOf(),
    "name" to ParserIdentifier(),
    "kind" to optionalKindParserOf(Symbol.COLON),
    "op" to ParserSymbol(Symbol.ASSIGN),
    "value" to ParserExpression(),
)

private fun variableOutcomeOf(values: Parsers) = AstVariable(
    name = values["name"],
    kind = values["kind"],
    value = values["value"],
    mutability = values["mutability"],
    visibility = values["visibility"],
    assignability = values["assignability"],
)

/**
 * Parses a list of function parameter definitions from the token stream.
 */
private fun typeParameterListParserOf(): Parser<List<AstTypeParameter>> =
    ParserPattern(::typeParameterListPatternOf) { it["parameters"] }

private fun typeParameterListPatternOf() = ParserSequence(
    "open_parenthesis" to ParserSymbol(Symbol.OPEN_BRACKET),
    "parameters" to ParserRepeating(typeParameterParserOf(), ParserSymbol(Symbol.COMMA)),
    "close_parenthesis" to ParserSymbol(Symbol.CLOSE_BRACKET),
)

/**
 * Parses a template declaration.
 */
private fun typeParameterParserOf(): Parser<AstTypeParameter> =
    ParserPattern(::typeParameterPatternOf, ::typeParameterOutcomeOf)

private fun typeParameterPatternOf() = ParserSequence(
    "name" to ParserIdentifier(),
    "colon" to ParserSymbol(Symbol.COLON),
    "kind" to kindParserOf(),
    "default" to ParserOptional(valueParserOf(Symbol.ASSIGN)),
)

private fun typeParameterOutcomeOf(values: Parsers) = AstTypeParameter(
    name = values["name"],
    kind = values["kind"],
    default = values["default"],
)
