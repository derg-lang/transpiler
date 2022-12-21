package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.source.lexeme.Identifier
import com.github.derg.transpiler.source.lexeme.Symbol
import com.github.derg.transpiler.source.lexeme.SymbolType
import com.github.derg.transpiler.source.lexeme.Token
import com.github.derg.transpiler.util.Result
import com.github.derg.transpiler.util.failureOf
import com.github.derg.transpiler.util.successOf

/**
 * Parses a single identifier from the provided token.
 */
class ParserName : Parser<Name>
{
    private var name: Name? = null
    
    override fun skipable(): Boolean = false
    override fun produce(): Name? = name
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (name != null)
            return successOf(ParseOk.Finished)
        
        val identifier = token as? Identifier ?: return failureOf(ParseError.UnexpectedToken(token))
        name = identifier.name
        return successOf(ParseOk.Complete)
    }
    
    override fun reset()
    {
        name = null
    }
}

/**
 * Parses a single symbol from the provided token. The parser will only accept one of the symbols present in the
 * [whitelist] when parsing. Any symbol not found in the whitelist is treated as an unexpected token.
 */
class ParserSymbol(vararg symbols: SymbolType) : Parser<SymbolType>
{
    private val whitelist = symbols.toSet()
    private var type: SymbolType? = null
    
    override fun skipable(): Boolean = false
    override fun produce(): SymbolType? = type
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (type != null)
            return successOf(ParseOk.Finished)
        
        val symbol = token as? Symbol ?: return failureOf(ParseError.UnexpectedToken(token))
        type = if (symbol.type in whitelist) symbol.type else return failureOf(ParseError.UnexpectedToken(token))
        return successOf(ParseOk.Complete)
    }
    
    override fun reset()
    {
        type = null
    }
}
