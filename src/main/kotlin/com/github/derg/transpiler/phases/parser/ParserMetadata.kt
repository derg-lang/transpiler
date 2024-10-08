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
fun mutabilityOf(symbol: Symbol?): Mutability = when (symbol)
{
    Symbol.MUTABLE -> Mutability.MUTABLE
    null           -> Mutability.IMMUTABLE
    else           -> throw IllegalStateException("Illegal symbol $symbol when parsing mutability")
}

/**
 * Determines the passability from the provided [symbol].
 */
fun passabilityOf(symbol: Symbol?): Passability = when (symbol)
{
    Symbol.IN     -> Passability.IN
    Symbol.OUT    -> Passability.OUT
    Symbol.MOVE   -> Passability.MOVE
    Symbol.BORROW -> Passability.BORROW
    null          -> Passability.IN
    else          -> throw IllegalStateException("Illegal symbol $symbol when parsing passability")
}

/**
 * Determines the assignability from the provided [symbol].
 */
fun assignabilityOf(symbol: Symbol): Assignability = when (symbol)
{
    Symbol.VALUE     -> Assignability.FINAL
    Symbol.VARYING   -> Assignability.ASSIGNABLE
    Symbol.REFERENCE -> Assignability.REFERENCE
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
    ParserOptional(ParserSymbol(Symbol.MUTABLE))

/**
 * Parses a passability from the token stream.
 */
fun passabilityParserOf(): Parser<Passability> =
    ParserPattern(::passabilityPatternOf, ::passabilityOf)

private fun passabilityPatternOf() =
    ParserOptional(ParserSymbol(Symbol.IN, Symbol.BORROW, Symbol.OUT, Symbol.MOVE))

/**
 * Parses an assignability from the token stream.
 */
fun assignabilityParserOf(): Parser<Assignability> =
    ParserPattern(::assignabilityPatternOf, ::assignabilityOf)

private fun assignabilityPatternOf() =
    ParserSymbol(Symbol.VALUE, Symbol.VARYING, Symbol.REFERENCE)

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
    "name" to ParserName(),
    "type" to typeParserOf(Symbol.COLON),
    "value" to ParserOptional(valueParserOf(Symbol.ASSIGN)),
)

private fun parameterOutcomeOf(values: Parsers) = AstParameter(
    name = values["name"],
    type = values["type"],
    value = values["value"],
    passability = values["passability"],
)

/**
 * Parses a type property definition from the token stream.
 */
fun propertyParserOf(): Parser<AstProperty> =
    ParserPattern(::propertyPatternOf, ::propertyOutcomeOf)

private fun propertyPatternOf() = ParserSequence(
    "visibility" to visibilityParserOf(),
    "assignability" to assignabilityParserOf(),
    "name" to ParserName(),
    "type" to typeParserOf(Symbol.COLON),
    "value" to ParserOptional(valueParserOf(Symbol.ASSIGN)),
)

private fun propertyOutcomeOf(values: Parsers) = AstProperty(
    name = values["name"],
    type = values["type"],
    value = values["value"],
    visibility = values["visibility"],
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

private fun segmentPatternOf() = ParserSequence(
    "imports" to ParserRepeating(nameParserOf(Symbol.USE)), // TODO: Use statements should allow modules to be aliased
    "definitions" to ParserRepeating(definitionParserOf()),
    "end" to ParserEnd,
)

private fun segmentOutcomeOf(values: Parsers) = AstSegment(
    imports = values["imports"],
    definitions = values["definitions"],
)

/**
 * Parses a type from the token stream.
 */
fun typeParserOf(symbol: Symbol): Parser<AstType> =
    ParserPattern({ typePatternOf(symbol) }, ::typeOutcomeOf)

private fun typePatternOf(symbol: Symbol) = ParserSequence(
    "colon" to ParserSymbol(symbol),
    "mutability" to mutabilityParserOf(),
    "name" to ParserName(),
)

private fun typeOutcomeOf(values: Parsers) = AstType(
    name = values["name"],
    mutability = values["mutability"],
)
