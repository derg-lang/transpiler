package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Expression
import com.github.derg.transpiler.ast.Mutability
import com.github.derg.transpiler.ast.Variable
import com.github.derg.transpiler.ast.Visibility
import com.github.derg.transpiler.core.Name
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
        "properties" to ParserAllOf(
            "visibility" to ParserOptional(ParserSymbol(SymbolType.PUB)),
            "mutability" to ParserSymbol(SymbolType.VAL, SymbolType.VAR, SymbolType.MUT),
        ),
        "name" to ParserName(),
        "op" to ParserSymbol(SymbolType.ASSIGN),
        "rhs" to ParserExpression(),
    )
    
    override fun produce(): Variable?
    {
        val values = parser.produce()
        val properties = values.produce<Parsers>("properties") ?: return null
        val visibility = visibilityOf(properties.produce("visibility"))
        val mutability = mutabilityOf(properties.produce("mutability") ?: return null)
        val name = values.produce<Name>("name") ?: return null
        val rhs = values.produce<Expression>("rhs") ?: return null
        return Variable(name = name, type = null, value = rhs, visibility = visibility, mutability = mutability)
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}
