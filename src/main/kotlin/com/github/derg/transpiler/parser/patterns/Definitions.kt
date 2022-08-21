package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.*
import com.github.derg.transpiler.ast.Function
import com.github.derg.transpiler.lexer.SymbolType
import com.github.derg.transpiler.lexer.Token
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.ParseOk
import com.github.derg.transpiler.parser.Parser
import com.github.derg.transpiler.util.Result

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
class ParserVariableDefinition : Parser<Variable>
{
    private val parser = ParserSequence(
        "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
        "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
        "name" to ParserName(),
        "op" to ParserSymbol(SymbolType.ASSIGN),
        "value" to ParserExpression(),
    )
    
    override fun produce(): Variable?
    {
        val values = parser.produce()
        return Variable(
            name = values.produce("name") ?: return null,
            type = null,
            value = values.produce("value") ?: return null,
            visibility = visibilityOf(values.produce("visibility")),
            mutability = mutabilityOf(values.produce("mutability") ?: return null),
        )
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a function definition from the provided token.
 */
class ParserFunctionDefinition : Parser<Function>
{
    private val parser = ParserSequence(
        "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
        "fun" to ParserSymbol(SymbolType.FUN),
        "name" to ParserName(),
        "open_parenthesis" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
        "parameters" to ParserRepeating(ParserFunctionParameterDefinition(), ParserSymbol(SymbolType.COMMA)),
        "close_parenthesis" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
        "error" to ParserOptional(ParserSequence("sym" to ParserSymbol(SymbolType.COLON), "type" to ParserName())),
        "value" to ParserOptional(ParserSequence("sym" to ParserSymbol(SymbolType.ARROW), "type" to ParserName())),
        "open_brace" to ParserSymbol(SymbolType.OPEN_BRACE),
        "close_brace" to ParserSymbol(SymbolType.CLOSE_BRACE),
    )
    
    override fun produce(): Function?
    {
        val values = parser.produce()
        return Function(
            name = values.produce("name") ?: return null,
            valueType = values.produce<Parsers>("value")?.produce("type"),
            errorType = values.produce<Parsers>("error")?.produce("type"),
            parameters = values.produce("parameters") ?: emptyList(),
            visibility = visibilityOf(values.produce("visibility")),
        )
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a function parameter definition from the provided token.
 */
private class ParserFunctionParameterDefinition : Parser<FunctionParameter>
{
    private val parser = ParserSequence(
        "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
        "name" to ParserName(),
        "type" to ParserOptional(ParserSequence("sym" to ParserSymbol(SymbolType.COLON), "type" to ParserName())),
        "val" to ParserOptional(ParserSequence("sym" to ParserSymbol(SymbolType.ASSIGN), "val" to ParserExpression())),
    )
    
    override fun produce(): FunctionParameter?
    {
        val values = parser.produce()
        return FunctionParameter(
            name = values.produce("name") ?: return null,
            type = values.produce<Parsers>("type")?.produce("type"),
            value = values.produce<Parsers>("bal")?.produce("val"),
            mutability = mutabilityOf(values.produce("mutability") ?: return null),
        )
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}
