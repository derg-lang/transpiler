package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*

/**
 * Determines the visibility from the provided [symbol].
 */
fun visibilityOf(symbol: Symbol?): Visibility = when (symbol)
{
    Symbol.EXPORTED  -> Visibility.EXPORTED
    Symbol.PRIVATE   -> Visibility.PRIVATE
    Symbol.PROTECTED -> Visibility.PROTECTED
    Symbol.PUBLIC    -> Visibility.PUBLIC
    null             -> Visibility.PRIVATE
    else             -> throw IllegalStateException("Illegal symbol $symbol when parsing visibility")
}

/**
 * Determines the mutability from the provided [symbol].
 */
fun mutabilityOf(symbol: Symbol): Mutability = when (symbol)
{
    Symbol.VALUE   -> Mutability.IMMUTABLE
    Symbol.VARYING -> Mutability.MUTABLE
    else           -> throw IllegalStateException("Illegal symbol $symbol when parsing mutability")
}

/**
 * Determines the passability from the provided [symbol].
 */
fun passabilityOf(symbol: Symbol?): Passability = when (symbol)
{
    Symbol.IN    -> Passability.IN
    Symbol.INOUT -> Passability.BORROW
    Symbol.OUT   -> Passability.OUT
    Symbol.MOVE  -> Passability.MOVE
    null         -> Passability.IN
    else         -> throw IllegalStateException("Illegal symbol $symbol when parsing passability")
}

/**
 * Determines the assignability from the provided [symbol].
 */
fun assignabilityOf(symbol: Symbol?): Assignability = when (symbol)
{
    Symbol.MUTABLE   -> Assignability.ASSIGNABLE
    Symbol.REFERENCE -> Assignability.REFERENCE
    null             -> Assignability.FINAL
    else             -> throw IllegalStateException("Illegal symbol $symbol when parsing assignability")
}

/**
 * Parses a symbol followed by an identifier. This operation is commonly used to specify optional type information or
 * to provide an optional name after a specific [symbol]. The output of the parser will always be the name of the found
 * identifier.
 */
fun nameParserOf(symbol: Symbol): Parser<String> =
    ParserPattern({ namePatternOf(symbol) }, { it["name"] })

private fun namePatternOf(symbol: Symbol) =
    ParserSequence("symbol" to ParserSymbol(symbol), "name" to ParserName())

/**
 * Parses a symbol followed by an expression.
 */
fun valueParserOf(symbol: Symbol): Parser<AstValue> =
    ParserPattern({ valuePatternOf(symbol) }, { it["expression"] })

private fun valuePatternOf(symbol: Symbol) =
    ParserSequence("symbol" to ParserSymbol(symbol), "expression" to expressionParserOf())

/**
 * Parses a visibility from the token stream.
 */
fun visibilityParserOf(): Parser<Visibility> =
    ParserPattern(::visibilityPatternOf, ::visibilityOf)

private fun visibilityPatternOf() =
    ParserOptional(ParserSymbol(Symbol.EXPORTED, Symbol.PRIVATE, Symbol.PROTECTED, Symbol.PUBLIC))

/**
 * Parses a mutability from the token stream.
 */
fun mutabilityParserOf(): Parser<Mutability> =
    ParserPattern(::mutabilityPatternOf, ::mutabilityOf)

private fun mutabilityPatternOf() =
    ParserSymbol(Symbol.VALUE, Symbol.VARYING)

/**
 * Parses a passability from the token stream.
 */
fun passabilityParserOf(): Parser<Passability> =
    ParserPattern(::passabilityPatternOf, ::passabilityOf)

private fun passabilityPatternOf() =
    ParserOptional(ParserSymbol(Symbol.IN, Symbol.INOUT, Symbol.OUT, Symbol.MOVE))

/**
 * Parses an assignability from the token stream.
 */
fun assignabilityParserOf(): Parser<Assignability> =
    ParserPattern(::assignabilityPatternOf, ::assignabilityOf)

private fun assignabilityPatternOf() =
    ParserOptional(ParserSymbol(Symbol.MUTABLE, Symbol.REFERENCE))

/**
 * Parses a function call argument from the token stream.
 */
fun argumentParserOf(): Parser<AstArgument> =
    ParserPattern(::argumentPatternOf, ::argumentOutcomeOf)

private fun argumentPatternOf() = ParserAnyOf(
    ParserSequence("expr" to expressionParserOf()),
    ParserSequence("name" to ParserName(), "sym" to ParserSymbol(Symbol.ASSIGN), "expr" to expressionParserOf()),
)

private fun argumentOutcomeOf(values: Parsers): AstArgument =
    AstArgument(values["name"], values["expr"])

/**
 * Parses a function parameter definition from the token stream.
 */
fun parameterParserOf(): Parser<AstParameter> =
    ParserPattern(::parameterPatternOf, ::parameterOutcomeOf)

private fun parameterPatternOf() = ParserSequence(
    "passability" to passabilityParserOf(),
    "assignability" to assignabilityParserOf(),
    "name" to ParserName(),
    "type" to ParserOptional(nameParserOf(Symbol.COLON)),
    "value" to ParserOptional(valueParserOf(Symbol.ASSIGN)),
)

private fun parameterOutcomeOf(values: Parsers) = AstParameter(
    name = values["name"],
    type = values["type"],
    value = values["value"],
    passability = values["passability"],
    assignability = values["assignability"],
)

/**
 * Parses a type property definition from the token stream.
 */
fun propertyParserOf(): Parser<AstProperty> =
    ParserPattern(::propertyPatternOf, ::propertyOutcomeOf)

private fun propertyPatternOf() = ParserSequence(
    "visibility" to visibilityParserOf(),
    "assignability" to assignabilityParserOf(),
    "mutability" to mutabilityParserOf(),
    "name" to ParserName(),
    "type" to nameParserOf(Symbol.COLON),
    "value" to ParserOptional(valueParserOf(Symbol.ASSIGN)),
)

private fun propertyOutcomeOf(values: Parsers) = AstProperty(
    name = values["name"],
    type = values["type"],
    value = values["value"],
    visibility = values["visibility"],
    mutability = values["mutability"],
    assignability = values["assignability"],
)

/**
 * Parses a single scope from the token stream.
 */
fun scopeParserOf(): Parser<List<AstInstruction>> =
    ParserPattern(::scopePatternOf, ::scopeOutcomeOf)

private fun scopePatternOf() = ParserAnyOf(
    ParserSequence("single" to statementParserOf()),
    ParserSequence(
        "open" to ParserSymbol(Symbol.OPEN_BRACE),
        "multiple" to ParserRepeating(statementParserOf()),
        "close" to ParserSymbol(Symbol.CLOSE_BRACE),
    )
)

private fun scopeOutcomeOf(values: Parsers): List<AstInstruction>
{
    val statement = values.get<AstInstruction?>("single")
    val statements = values.get<List<AstInstruction>?>("multiple")
    
    return when
    {
        statement != null  -> listOf(statement)
        statements != null -> statements
        else               -> emptyList()
    }
}

/**
 * Parses a segment definition from the token stream.
 */
fun segmentParserOf(): Parser<AstSegment> =
    ParserPattern(::segmentPatternOf, ::segmentOutcomeOf)

// TODO: Use statements should allow modules to be imported into namespaces
private fun segmentPatternOf() = ParserSequence(
    "module" to ParserOptional(nameParserOf(Symbol.MODULE)),
    "imports" to ParserRepeating(nameParserOf(Symbol.USE)),
    "definitions" to ParserRepeating(definitionParserOf()),
    "end" to ParserEnd,
)

private fun segmentOutcomeOf(values: Parsers) = AstSegment(
    module = values["module"],
    imports = values["imports"],
    definitions = values["definitions"],
)
