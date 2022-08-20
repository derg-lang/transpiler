package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.lexer.SymbolType
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.Tester
import org.junit.jupiter.api.Test

class TestParserName
{
    private val tester = Tester { ParserName() }
    
    @Test
    fun `Given valid token, when parsing, then correct product`()
    {
        tester.parse("foo").isOk(1).isDone().isValue("foo").resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse(",").isBad { ParseError.UnexpectedToken(it[0]) }
    }
}

class TestParserSymbol
{
    private val tester = Tester { ParserSymbol(SymbolType.AND, SymbolType.OR) }
    
    @Test
    fun `Given valid token, when parsing, then correct product`()
    {
        tester.parse("&&").isOk(1).isDone().isValue(SymbolType.AND).resets()
        tester.parse("||").isOk(1).isDone().isValue(SymbolType.OR).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("^^").isBad { ParseError.UnexpectedToken(it[0]) }
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
    }
}
