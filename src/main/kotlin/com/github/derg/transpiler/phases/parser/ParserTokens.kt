package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.ast.Expression
import com.github.derg.transpiler.source.ast.Constant
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.util.Result
import com.github.derg.transpiler.util.failureOf
import com.github.derg.transpiler.util.successOf
import com.github.derg.transpiler.util.toSuccess

/**
 * Parses a single identifier from the token stream.
 */
class ParserName : Parser<Name>
{
    private var name: Name? = null
    
    override fun skipable(): Boolean = false
    override fun produce(): Name = name ?: throw IllegalStateException("No name has been parsed")
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
 * Parses a single symbol from the token stream. The parser will only accept one of the symbols present in the
 * [whitelist] when parsing. Any symbol not found in the whitelist is treated as an unexpected token.
 */
class ParserSymbol(vararg symbols: SymbolType) : Parser<SymbolType>
{
    private val whitelist = symbols.toSet()
    private var type: SymbolType? = null
    
    override fun skipable(): Boolean = false
    override fun produce(): SymbolType = type ?: throw IllegalStateException("No symbol has been parsed")
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

/**
 * Parses a single boolean value from the token stream.
 */
class ParserBool : Parser<Expression>
{
    private var expression: Expression? = null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (expression != null)
            return successOf(ParseOk.Finished)
        
        val symbol = token as? Symbol ?: return failureOf(ParseError.UnexpectedToken(token))
        expression = when (symbol.type)
        {
            SymbolType.TRUE  -> Constant.Bool(true)
            SymbolType.FALSE -> Constant.Bool(false)
            else             -> return failureOf(ParseError.UnexpectedToken(token))
        }
        return successOf(ParseOk.Complete)
    }
    
    override fun skipable(): Boolean = false
    override fun produce(): Expression = expression ?: throw IllegalStateException("No expression has been parsed")
    override fun reset()
    {
        expression = null
    }
}

/**
 * Parses a single numeric value from the token stream.
 */
class ParserReal : Parser<Expression>
{
    private var expression: Expression? = null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (expression != null)
            return successOf(ParseOk.Finished)
        
        val number = token as? Numeric ?: return failureOf(ParseError.UnexpectedToken(token))
        expression = Constant.Real(number.value, number.type)
        return successOf(ParseOk.Complete)
    }
    
    override fun skipable(): Boolean = false
    override fun produce(): Expression = expression ?: throw IllegalStateException("No expression has been parsed")
    override fun reset()
    {
        expression = null
    }
}

/**
 * Parses a single string value from the token stream.
 */
class ParserText : Parser<Expression>
{
    private var expression: Expression? = null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (expression != null)
            return successOf(ParseOk.Finished)
        
        val string = token as? Textual ?: return failureOf(ParseError.UnexpectedToken(token))
        expression = Constant.Text(string.value, string.type)
        return successOf(ParseOk.Complete)
    }
    
    override fun skipable(): Boolean = false
    override fun produce(): Expression = expression ?: throw IllegalStateException("No expression has been parsed")
    override fun reset()
    {
        expression = null
    }
}
