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
    SymbolType.PUB -> Visibility.PUBLIC
    null           -> Visibility.PRIVATE
    else           -> throw IllegalStateException("Illegal symbol $symbol when parsing variable visibility")
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
    ParserPattern({ namePatternOf(symbol) }, { it.produce("name") })

private fun namePatternOf(symbol: SymbolType) =
    ParserSequence("symbol" to ParserSymbol(symbol), "name" to ParserName())

/**
 * Parses a symbol followed by an expression.
 */
fun valueParserOf(symbol: SymbolType): Parser<Expression> =
    ParserPattern({ valuePatternOf(symbol) }, { it.produce("expression") })

private fun valuePatternOf(symbol: SymbolType) =
    ParserSequence("symbol" to ParserSymbol(symbol), "expression" to expressionParserOf())

/**
 * Parses a function call argument from the token stream.
 */
fun argumentParserOf(): Parser<Argument> =
    ParserPattern(::argumentPatternOf, ::argumentOutcomeOf)

private fun argumentPatternOf() = ParserAnyOf(
    ParserSequence("expr" to expressionParserOf()),
    ParserSequence("name" to ParserName(), "sym" to ParserSymbol(SymbolType.ASSIGN), "expr" to expressionParserOf()),
)

private fun argumentOutcomeOf(values: Parsers): Argument?
{
    val name = values.produce<Name>("name")
    val expression = values.produce<Expression>("expr") ?: return null
    return Argument(name, expression)
}

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

private fun parameterOutcomeOf(values: Parsers): Parameter?
{
    return Parameter(
        name = values.produce("name") ?: return null,
        type = values.produce("type"),
        value = values.produce("value"),
        mutability = mutabilityOf(values.produce("mutability") ?: return null),
    )
}

/**
 * Parses a type property definition from the token stream.
 */
fun propertyParserOf(): Parser<Property> =
    ParserPattern(::propertyPatternOf, ::propertyOutcomeOf)

private fun propertyPatternOf() = ParserSequence(
    "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
    "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
    "name" to ParserName(),
    "type" to ParserOptional(nameParserOf(SymbolType.COLON)),
    "value" to ParserOptional(valueParserOf(SymbolType.ASSIGN)),
)

private fun propertyOutcomeOf(values: Parsers): Property?
{
    return Property(
        name = values.produce("name") ?: return null,
        type = values.produce("type"),
        value = values.produce("value"),
        visibility = visibilityOf(values.produce("visibility")),
        mutability = mutabilityOf(values.produce("mutability") ?: return null)
    )
}

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

private fun scopeOutcomeOf(values: Parsers): Scope?
{
    val statement = values.produce<Statement>("single")
    val statements = values.produce<List<Statement>>("multiple")
    val isBraced = statement == null
    return Scope(isBraced, statements ?: statement?.let { listOf(it) } ?: return null)
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

private fun segmentOutcomeOf(values: Parsers): Segment
{
    return Segment(
        module = values.produce("module"),
        imports = values.produce<List<Name>>("imports")?.toSet() ?: emptySet(),
        definitions = values.produce("statements") ?: emptyList(),
    )
}
