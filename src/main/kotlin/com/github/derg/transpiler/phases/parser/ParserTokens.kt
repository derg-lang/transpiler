package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * Parses a single identifier from the token stream.
 */
class ParserName : Parser<String>
{
    private var name: String? = null
    
    override fun skipable(): Boolean = false
    override fun produce(): String = name ?: throw IllegalStateException("No name has been parsed")
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (name != null)
            return ParseOk.Finished.toSuccess()
        
        val identifier = token as? Identifier ?: return ParseError.UnexpectedToken(token).toFailure()
        name = identifier.name
        return ParseOk.Complete.toSuccess()
    }
    
    override fun reset()
    {
        name = null
    }
}

/**
 * Parses a single symbol from the token stream. The parser will only accept one of the symbols present in the
 * [whitelist] when parsing. Any symbol not found in the whitelist is treated as an unexpected token.
 */
class ParserSymbol(vararg symbols: Symbol) : Parser<Symbol>
{
    private val whitelist = symbols.toSet()
    private var type: Symbol? = null
    
    override fun skipable(): Boolean = false
    override fun produce(): Symbol = type ?: throw IllegalStateException("No symbol has been parsed")
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (type != null)
            return ParseOk.Finished.toSuccess()
        
        val keyword = token as? Keyword ?: return ParseError.UnexpectedToken(token).toFailure()
        type = if (keyword.type in whitelist) keyword.type else return ParseError.UnexpectedToken(token).toFailure()
        return ParseOk.Complete.toSuccess()
    }
    
    override fun reset()
    {
        type = null
    }
}

/**
 * Parses a single boolean value from the token stream.
 */
class ParserBool : Parser<AstExpression>
{
    private var expression: AstExpression? = null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (expression != null)
            return ParseOk.Finished.toSuccess()
        
        val keyword = token as? Keyword ?: return ParseError.UnexpectedToken(token).toFailure()
        expression = when (keyword.type)
        {
            Symbol.TRUE  -> AstBool(true)
            Symbol.FALSE -> AstBool(false)
            else         -> return ParseError.UnexpectedToken(token).toFailure()
        }
        return ParseOk.Complete.toSuccess()
    }
    
    override fun skipable(): Boolean = false
    override fun produce(): AstExpression = expression ?: throw IllegalStateException("No expression has been parsed")
    override fun reset()
    {
        expression = null
    }
}

/**
 * Parses a single numeric value from the token stream.
 */
class ParserReal : Parser<AstExpression>
{
    private var expression: AstExpression? = null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (expression != null)
            return ParseOk.Finished.toSuccess()
        
        val number = token as? Numeric ?: return ParseError.UnexpectedToken(token).toFailure()
        expression = AstReal(number.value, number.type ?: Builtin.INT32_LIT.name)
        return ParseOk.Complete.toSuccess()
    }
    
    override fun skipable(): Boolean = false
    override fun produce(): AstExpression = expression ?: throw IllegalStateException("No expression has been parsed")
    override fun reset()
    {
        expression = null
    }
}

/**
 * Parses a single string value from the token stream.
 */
class ParserText : Parser<AstExpression>
{
    private var expression: AstExpression? = null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (expression != null)
            return ParseOk.Finished.toSuccess()
        
        val string = token as? Textual ?: return ParseError.UnexpectedToken(token).toFailure()
        expression = AstText(string.value, string.type ?: Builtin.STR_LIT.name)
        return ParseOk.Complete.toSuccess()
    }
    
    override fun skipable(): Boolean = false
    override fun produce(): AstExpression = expression ?: throw IllegalStateException("No expression has been parsed")
    override fun reset()
    {
        expression = null
    }
}

/**
 * Parses the end of the token stream. Can only be used to represent the end of the token stream.
 */
object ParserEnd : Parser<Unit>
{
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (token !is EndOfFile)
            return ParseError.UnexpectedToken(token).toFailure()
        return ParseOk.Finished.toSuccess()
    }
    
    override fun skipable(): Boolean = false
    override fun produce() = Unit
    override fun reset()
    {
    }
}
