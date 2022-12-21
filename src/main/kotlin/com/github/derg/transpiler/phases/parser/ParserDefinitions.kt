package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.source.Mutability
import com.github.derg.transpiler.source.Visibility
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.ast.Function
import com.github.derg.transpiler.source.lexeme.SymbolType

/**
 * Determines the given visibility from the provided [symbol].
 */
private fun visibilityOf(symbol: SymbolType?): Visibility = when (symbol)
{
    SymbolType.PUB -> Visibility.PUBLIC
    null           -> Visibility.PRIVATE
    else           -> throw IllegalStateException("Illegal symbol $symbol when parsing variable visibility")
}

/**
 * Determines the given mutability from the provided [symbol].
 */
private fun mutabilityOf(symbol: SymbolType): Mutability = when (symbol)
{
    SymbolType.VAL -> Mutability.VALUE
    SymbolType.VAR -> Mutability.VARYING
    SymbolType.MUT -> Mutability.MUTABLE
    else           -> throw IllegalStateException("Illegal symbol $symbol when parsing variable mutability")
}

/**
 * Parses a variable definition from the provided token.
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

private fun variableOutcomeOf(values: Parsers): Variable?
{
    return Variable(
        name = values.produce("name") ?: return null,
        type = null,
        value = values.produce("value") ?: return null,
        visibility = visibilityOf(values.produce("visibility")),
        mutability = mutabilityOf(values.produce("mutability") ?: return null),
    )
}

/**
 * Parses a function definition from the provided token.
 */
fun functionParserOf(): Parser<Statement> =
    ParserPattern(::functionPatternOf, ::functionOutcomeOf)

private fun functionPatternOf() = ParserSequence(
    "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
    "fun" to ParserSymbol(SymbolType.FUN),
    "name" to ParserName(),
    "open_parenthesis" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
    "parameters" to ParserRepeating(functionParameterParser(), ParserSymbol(SymbolType.COMMA)),
    "close_parenthesis" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
    "error" to ParserOptional(ParserSequence("sym" to ParserSymbol(SymbolType.COLON), "type" to ParserName())),
    "value" to ParserOptional(ParserSequence("sym" to ParserSymbol(SymbolType.ARROW), "type" to ParserName())),
    "open_brace" to ParserSymbol(SymbolType.OPEN_BRACE),
    "statements" to ParserRepeating(statementParserOf()),
    "close_brace" to ParserSymbol(SymbolType.CLOSE_BRACE),
)

private fun functionOutcomeOf(values: Parsers): Function?
{
    return Function(
        name = values.produce("name") ?: return null,
        valueType = values.produce<Parsers>("value")?.produce("type"),
        errorType = values.produce<Parsers>("error")?.produce("type"),
        parameters = values.produce("parameters") ?: return null,
        visibility = visibilityOf(values.produce("visibility")),
        scope = Scope(true, values.produce("statements") ?: return null),
    )
}

/**
 * Parses a function parameter definition from the provided token.
 */
private fun functionParameterParser(): Parser<Function.Parameter> =
    ParserPattern(::functionParameterPattern, ::functionParameterOutcome)

private fun functionParameterPattern() = ParserSequence(
    "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
    "name" to ParserName(),
    "type" to ParserOptional(ParserSequence("sym" to ParserSymbol(SymbolType.COLON), "type" to ParserName())),
    "val" to ParserOptional(ParserSequence("sym" to ParserSymbol(SymbolType.ASSIGN), "val" to expressionParserOf())),
)

private fun functionParameterOutcome(values: Parsers): Function.Parameter?
{
    return Function.Parameter(
        name = values.produce("name") ?: return null,
        type = values.produce<Parsers>("type")?.produce("type"),
        value = values.produce<Parsers>("bal")?.produce("val"),
        mutability = mutabilityOf(values.produce("mutability") ?: return null),
    )
}

/**
 *
 */
fun segmentParser(): Parser<Segment> =
    ParserPattern(::segmentPattern, ::segmentOutcome)

// TODO: Use statements should allow modules to be imported into namespaces
private fun segmentPattern() = ParserSequence(
    "module" to ParserOptional(ParserSequence("sym" to ParserSymbol(SymbolType.MODULE), "name" to ParserName())),
    "imports" to ParserRepeating(ParserSequence("sym" to ParserSymbol(SymbolType.USE), "name" to ParserName())),
    "statements" to ParserRepeating(statementParserOf()),
)

private fun segmentOutcome(values: Parsers): Segment
{
    return Segment(
        module = values.produce<Parsers>("module")?.produce<Name>("name"),
        imports = values.produce<List<Parsers>>("imports")?.mapNotNull { it.produce<Name>("name") }?.toSet() ?: emptySet(),
        statements = values.produce("statements") ?: emptyList(),
    )
}
