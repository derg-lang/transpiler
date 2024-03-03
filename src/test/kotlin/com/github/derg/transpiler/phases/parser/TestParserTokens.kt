package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import org.junit.jupiter.api.*

class TestParserIdentifier
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
    private val tester = Tester { ParserSymbol(Symbol.AND, Symbol.OR) }
    
    @Test
    fun `Given valid token, when parsing, then correct product`()
    {
        tester.parse("&&").isOk(1).isDone().isValue(Symbol.AND).resets()
        tester.parse("||").isOk(1).isDone().isValue(Symbol.OR).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("^^").isBad { ParseError.UnexpectedToken(it[0]) }
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
    }
}

class TestParserBool
{
    private val tester = Tester { ParserBool() }
    
    @Test
    fun `Given valid token, when parsing, then correct product`()
    {
        tester.parse("true").isOk(1).isDone().isValue(true.ast).resets()
        tester.parse("false").isOk(1).isDone().isValue(false.ast).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("if").isBad { ParseError.UnexpectedToken(it[0]) }
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
    }
}

class TestParserReal
{
    private val tester = Tester { ParserReal() }
    
    @Test
    fun `Given valid token, when parsing, then correct product`()
    {
        tester.parse("4").isOk(1).isDone().isValue(4.ast).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
    }
}

class TestParserText
{
    private val tester = Tester { ParserText() }
    
    @Test
    fun `Given valid token, when parsing, then correct product`()
    {
        tester.parse("\"\"").isOk(1).isDone().isValue("".ast).resets()
        tester.parse("\"foo\"").isOk(1).isDone().isValue("foo".ast).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
    }
}

class TestParserEnd
{
    private val tester = Tester { ParserEnd }
    
    @Test
    fun `Given valid token, when parsing, then correct product`()
    {
        tester.parse("").isDone()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
    }
}
