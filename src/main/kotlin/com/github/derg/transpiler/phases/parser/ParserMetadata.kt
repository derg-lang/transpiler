package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.Mutability
import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.Visibility
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.SymbolType

/**
 * Determines the given visibility from the provided [symbol].
 */
fun visibilityOf(symbol: SymbolType?): Visibility = when (symbol)
{
    SymbolType.EXPORTED  -> Visibility.EXPORTED
    SymbolType.PRIVATE   -> Visibility.PRIVATE
    SymbolType.PROTECTED -> Visibility.PROTECTED
    SymbolType.PUBLIC    -> Visibility.PUBLIC
    null                 -> Visibility.PRIVATE
    else                 -> throw IllegalStateException("Illegal symbol $symbol when parsing variable visibility")
}

/**
 * Determines the given mutability from the provided [symbol].
 */
fun mutabilityOf(symbol: SymbolType): Mutability = when (symbol)
{
    SymbolType.VAL -> Mutability.VALUE
    SymbolType.VAR -> Mutability.VARYING
    SymbolType.MUT -> Mutability.MUTABLE
    else           -> throw IllegalStateException("Illegal symbol $symbol when parsing variable mutability")
}

/**
 * Parses a symbol followed by an identifier. This operation is commonly used to specify optional type information or
 * to provide an optional name after a specific [symbol]. The output of the parser will always be the name of the found
 * identifier.
 */
fun nameParserOf(symbol: SymbolType): Parser<Name> =
    ParserPattern({ namePatternOf(symbol) }, { it["name"] })

private fun namePatternOf(symbol: SymbolType) =
    ParserSequence("symbol" to ParserSymbol(symbol), "name" to ParserName())

/**
 * Parses a symbol followed by an expression.
 */
fun valueParserOf(symbol: SymbolType): Parser<Expression> =
    ParserPattern({ valuePatternOf(symbol) }, { it["expression"] })

private fun valuePatternOf(symbol: SymbolType) =
    ParserSequence("symbol" to ParserSymbol(symbol), "expression" to expressionParserOf())

/**
 * Parses a visibility from the token stream.
 */
fun visibilityParserOf(): Parser<Visibility> =
    ParserPattern(::visibilityPatternOf, ::visibilityOf)

private fun visibilityPatternOf() = ParserOptional(
    ParserSymbol(SymbolType.EXPORTED, SymbolType.PRIVATE, SymbolType.PROTECTED, SymbolType.PUBLIC),
    SymbolType.PRIVATE
)

/**
 * Parses a function call argument from the token stream.
 */
fun argumentParserOf(): Parser<Argument> =
    ParserPattern(::argumentPatternOf, ::argumentOutcomeOf)

private fun argumentPatternOf() = ParserAnyOf(
    ParserSequence("expr" to expressionParserOf()),
    ParserSequence("name" to ParserName(), "sym" to ParserSymbol(SymbolType.ASSIGN), "expr" to expressionParserOf()),
)

private fun argumentOutcomeOf(values: Parsers): Argument =
    Argument(values["name"], values["expr"])

/**
 * Parses a function parameter definition from the token stream.
 */
fun parameterParserOf(): Parser<Parameter> =
    ParserPattern(::parameterPatternOf, ::parameterOutcomeOf)

private fun parameterPatternOf() = ParserSequence(
    "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
    "name" to ParserName(),
    "type" to ParserOptional(nameParserOf(SymbolType.COLON)),
    "value" to ParserOptional(valueParserOf(SymbolType.ASSIGN)),
)

private fun parameterOutcomeOf(values: Parsers) = Parameter(
    name = values["name"],
    type = values["type"],
    value = values["value"],
    mutability = mutabilityOf(values["mutability"]),
)

/**
 * Parses a type property definition from the token stream.
 */
fun propertyParserOf(): Parser<Property> =
    ParserPattern(::propertyPatternOf, ::propertyOutcomeOf)

private fun propertyPatternOf() = ParserSequence(
    "visibility" to visibilityParserOf(),
    "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
    "name" to ParserName(),
    "type" to ParserOptional(nameParserOf(SymbolType.COLON)),
    "value" to ParserOptional(valueParserOf(SymbolType.ASSIGN)),
)

private fun propertyOutcomeOf(values: Parsers) = Property(
    name = values["name"],
    type = values["type"],
    value = values["value"],
    visibility = values["visibility"],
    mutability = mutabilityOf(values["mutability"]),
)

/**
 * Parses a single scope from the token stream.
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

private fun scopeOutcomeOf(values: Parsers): Scope
{
    val statement = values.get<Statement?>("single")
    val statements = values.get<List<Statement>?>("multiple")
    
    return when
    {
        statement != null  -> Scope(false, listOf(statement))
        statements != null -> Scope(true, statements)
        else               -> Scope(false, emptyList())
    }
}

/**
 * Parses a segment definition from the token stream.
 */
fun segmentParserOf(): Parser<Segment> =
    ParserPattern(::segmentPatternOf, ::segmentOutcomeOf)

// TODO: Use statements should allow modules to be imported into namespaces
private fun segmentPatternOf() = ParserSequence(
    "module" to ParserOptional(nameParserOf(SymbolType.MODULE)),
    "imports" to ParserRepeating(nameParserOf(SymbolType.USE)),
    "statements" to ParserRepeating(statementParserOf()),
)

private fun segmentOutcomeOf(values: Parsers) = Segment(
    module = values["module"],
    imports = values["imports"],
    definitions = values["statements"],
)
