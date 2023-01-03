package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.Mutability
import com.github.derg.transpiler.source.Name
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
 * Parses a symbol followed by an identifier. This operation is commonly used to specify optional type information or
 * to provide an optional name after a specific [symbol]. The output of the parser will always be the name of the found
 * identifier.
 */
private fun nameParserOf(symbol: SymbolType): Parser<Name> =
    ParserPattern({ namePatternOf(symbol) }, { it.produce("name") })

private fun namePatternOf(symbol: SymbolType) =
    ParserSequence("symbol" to ParserSymbol(symbol), "name" to ParserName())

/**
 * Parses a symbol followed by an expression.
 */
private fun valueParserOf(symbol: SymbolType): Parser<Expression> =
    ParserPattern({ valuePatternOf(symbol) }, { it.produce("expression") })

private fun valuePatternOf(symbol: SymbolType) =
    ParserSequence("symbol" to ParserSymbol(symbol), "expression" to expressionParserOf())

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
 * Parses a function definition from the token stream.
 */
fun functionParserOf(): Parser<Statement> =
    ParserPattern(::functionPatternOf, ::functionOutcomeOf)

private fun functionPatternOf() = ParserSequence(
    "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
    "fun" to ParserSymbol(SymbolType.FUN),
    "name" to ParserName(),
    "open_parenthesis" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
    "parameters" to ParserRepeating(functionParameterParserOf(), ParserSymbol(SymbolType.COMMA)),
    "close_parenthesis" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
    "error" to ParserOptional(nameParserOf(SymbolType.COLON)),
    "value" to ParserOptional(nameParserOf(SymbolType.ARROW)),
    "open_brace" to ParserSymbol(SymbolType.OPEN_BRACE),
    "statements" to ParserRepeating(statementParserOf()),
    "close_brace" to ParserSymbol(SymbolType.CLOSE_BRACE),
)

private fun functionOutcomeOf(values: Parsers): Function?
{
    return Function(
        name = values.produce("name") ?: return null,
        valueType = values.produce("value"),
        errorType = values.produce("error"),
        parameters = values.produce("parameters") ?: return null,
        visibility = visibilityOf(values.produce("visibility")),
        scope = Scope(true, values.produce("statements") ?: return null),
    )
}

/**
 * Parses a function parameter definition from the token stream.
 */
private fun functionParameterParserOf(): Parser<Function.Parameter> =
    ParserPattern(::functionParameterPatternOf, ::functionParameterOutcomeOf)

private fun functionParameterPatternOf() = ParserSequence(
    "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
    "name" to ParserName(),
    "type" to ParserOptional(nameParserOf(SymbolType.COLON)),
    "value" to ParserOptional(valueParserOf(SymbolType.ASSIGN)),
)

private fun functionParameterOutcomeOf(values: Parsers): Function.Parameter?
{
    return Function.Parameter(
        name = values.produce("name") ?: return null,
        type = values.produce("type"),
        value = values.produce("value"),
        mutability = mutabilityOf(values.produce("mutability") ?: return null),
    )
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
        statements = values.produce("statements") ?: emptyList(),
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
    "properties" to ParserRepeating(typePropertyParserOf()),
    "close_brace" to ParserSymbol(SymbolType.CLOSE_BRACE),
)

private fun typeOutcomeOf(values: Parsers): Type?
{
    return Type(
        name = values.produce("name") ?: return null,
        visibility = visibilityOf(values.produce("visibility")),
        properties = values.produce("properties") ?: emptyList(),
    )
}

/**
 * Parses a type property definition from the token stream.
 */
private fun typePropertyParserOf(): Parser<Type.Property> =
    ParserPattern(::typePropertyPatternOf, ::typePropertyOutcomeOf)

private fun typePropertyPatternOf() = ParserSequence(
    "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
    "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
    "name" to ParserName(),
    "type" to ParserOptional(nameParserOf(SymbolType.COLON)),
    "value" to ParserOptional(valueParserOf(SymbolType.ASSIGN)),
)

private fun typePropertyOutcomeOf(values: Parsers): Type.Property?
{
    return Type.Property(
        name = values.produce("name") ?: return null,
        type = values.produce("type"),
        value = values.produce("value"),
        visibility = visibilityOf(values.produce("visibility")),
        mutability = mutabilityOf(values.produce("mutability") ?: return null)
    )
}
