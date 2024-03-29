package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*
import java.math.*

private const val INT = "int"
private const val BOOL = "bool"
private const val TEXT = "text"

class TestParserAnyOf
{
    @Test
    fun `Given valid token, when parsing empty, then finished`()
    {
        val tester = Tester { ParserAnyOf<Unit>() }
        
        tester.parse("").isDone()
    }
    
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserAnyOf(ParserInteger(), ParserBool()) }
        
        tester.parse("42").isOk(1).isDone().isValue(42.ast).resets()
        tester.parse("true").isOk(1).isDone().isValue(true.ast).resets()
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserAnyOf(
                ParserSequence(INT to ParserInteger(), BOOL to ParserBool()),
                ParserSequence(TEXT to ParserText(), BOOL to ParserBool()),
                ParserSequence(TEXT to ParserText()),
            )
        }
        
        tester.parse("42 true").isWip(1).isOk(1).isDone()
            .hasValue(INT, 42.ast).hasValue(BOOL, true.ast).resets()
        tester.parse("\"foo\" false").isOk(2).isDone()
            .hasValue(TEXT, "foo".ast).hasValue(BOOL, false.ast).resets()
        tester.parse("\"foo\"").isOk(1).isDone()
            .hasValue(TEXT, "foo".ast).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserAnyOf(ParserInteger(), ParserBool()) }
        
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
    }
}

class TestParserAllOf
{
    @Test
    fun `Given valid token, when parsing empty, then finished`()
    {
        val tester = Tester { ParserAllOf() }
        
        tester.parse("").isDone()
    }
    
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserAllOf(INT to ParserInteger(), BOOL to ParserBool()) }
        
        tester.parse("42 true").isWip(1).isOk(1).isDone()
            .hasValue(INT, 42.ast).hasValue(BOOL, true.ast)
        tester.parse("true 42").isWip(1).isOk(1).isDone()
            .hasValue(INT, 42.ast).hasValue(BOOL, true.ast)
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserAllOf(
                INT to ParserSequence("a" to ParserInteger(), "b" to ParserInteger()),
                BOOL to ParserBool(),
                TEXT to ParserOptional(ParserText()),
            )
        }
        
        tester.parse("1 2 true").isWip(2).isOk(1).isDone()
            .hasValue(INT, "a" to 1.ast, "b" to 2.ast)
            .hasValue(BOOL, true.ast)
        tester.parse("\"foo\" 1 2 true").isWip(3).isOk(1).isDone()
            .hasValue(INT, "a" to 1.ast, "b" to 2.ast)
            .hasValue(BOOL, true.ast)
            .hasValue(TEXT, "foo".ast)
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserAllOf(INT to ParserInteger(), BOOL to ParserBool()) }
        
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
        tester.parse("42").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("true").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserSequence
{
    @Test
    fun `Given valid token, when parsing empty, then finished`()
    {
        val tester = Tester { ParserSequence() }
        
        tester.parse("").isDone()
    }
    
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester()
        {
            ParserSequence(
                INT to ParserInteger(),
                BOOL to ParserBool(),
                TEXT to ParserText(),
            )
        }
        
        tester.parse("1 true \"foo\"").isWip(2).isOk(1).isDone()
            .hasValue(INT, 1.ast).hasValue(BOOL, true.ast).hasValue(TEXT, "foo".ast)
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserSequence(
                INT to ParserSequence("a" to ParserInteger(), "b" to ParserInteger()),
                BOOL to ParserSequence("a" to ParserBool(), "b" to ParserBool()),
                TEXT to ParserOptional(ParserText()),
            )
        }
        
        tester.parse("1 2 true false").isWip(3).isOk(1).isDone()
            .hasValue(INT, "a" to 1.ast, "b" to 2.ast)
            .hasValue(BOOL, "a" to true.ast, "b" to false.ast)
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserSequence(INT to ParserInteger(), BOOL to ParserBool()) }
        
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
        tester.parse("1").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserRepeating
{
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserRepeating(ParserInteger(), ParserSymbol(Symbol.COMMA)) }
        
        tester.parse("").isDone().isValue(emptyList())
        tester.parse("1").isOk(1).isDone().isValue(listOf(1.ast)).resets()
        tester.parse("1,").isOk(2).isDone().isValue(listOf(1.ast)).resets()
        tester.parse("1,2").isOk(3).isDone().isValue(listOf(1.ast, 2.ast)).resets()
        tester.parse("1,2,").isOk(4).isDone().isValue(listOf(1.ast, 2.ast)).resets()
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserRepeating(
                ParserSequence(INT to ParserInteger(), BOOL to ParserBool()),
                ParserSymbol(Symbol.COMMA),
            )
        }
        
        tester.parse("1 true").isWip(1).isOk(1).isDone()
            .hasValue(INT, 1.ast).hasValue(BOOL, true.ast)
        tester.parse("1 true,").isWip(1).isOk(2).isDone()
            .hasValue(INT, 1.ast).hasValue(BOOL, true.ast)
        tester.parse("1 true, 2 false").isWip(1).isOk(2).isWip(1).isOk(1).isDone()
            .hasValue(INT, 1.ast, 2.ast).hasValue(BOOL, true.ast, false.ast)
        tester.parse("1 true, 2 false,").isWip(1).isOk(2).isWip(1).isOk(2).isDone()
            .hasValue(INT, 1.ast, 2.ast).hasValue(BOOL, true.ast, false.ast)
    }
    
    @Test
    fun `Given valid token, when parsing empty separator, then value produced`()
    {
        val tester = Tester { ParserRepeating(ParserSequence(INT to ParserInteger(), BOOL to ParserBool())) }
        
        tester.parse("1 true").isWip(1).isOk(1).isDone()
            .hasValue(INT, 1.ast)
        tester.parse("1 true 2 false").isWip(1).isOk(1).isWip(1).isOk(1).isDone()
            .hasValue(INT, 1.ast, 2.ast).hasValue(BOOL, true.ast, false.ast)
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester()
        {
            ParserRepeating(
                ParserSequence(INT to ParserInteger(), BOOL to ParserBool()),
                ParserSymbol(Symbol.COMMA),
            )
        }
        
        tester.parse("1").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("1 true, 2").isWip(1).isOk(2).isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
    
    @Test
    fun `Given produced values, when resetting, then values are retained`()
    {
        val parser = ParserRepeating(ParserInteger())
            .also { it.parse(Numeric(BigInteger.ONE, null)) }
            .also { it.parse(EndOfFile) }
        val items = parser.produce()
        
        assertTrue(items.isNotEmpty())
        parser.reset()
        assertTrue(items.isNotEmpty())
    }
}

class TestParserOptional
{
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserOptional(ParserInteger()) }
        
        tester.parse("").isDone().isValue(null).resets()
        tester.parse("42").isOk(1).isDone().isValue(42.ast).resets()
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester { ParserOptional(ParserSequence(INT to ParserInteger(), BOOL to ParserBool())) }
        
        tester.parse("").isDone()
            .hasValue(INT, null).hasValue(BOOL, null)
        tester.parse("42 true").isWip(1).isOk(1).isDone()
            .hasValue(INT, 42.ast).hasValue(BOOL, true.ast)
    }
    
    @Test
    fun `Given missing token, when parsing simple, then default produced`()
    {
        val tester = Tester { ParserOptional(ParserInteger(), 7.ast) }
        
        tester.parse("").isDone().isValue(7.ast).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserOptional(ParserSequence(INT to ParserInteger(), BOOL to ParserBool())) }
        
        tester.parse("1 bah").isWip(1).isBad { ParseError.UnexpectedToken(it[1]) }
    }
}

class TestParserPattern
{
    private fun pattern() = ParserSequence("lhs" to ParserInteger(), "rhs" to ParserInteger())
    private fun converter(outcome: Parsers) = AstAdd(outcome["lhs"], outcome["rhs"])
    
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserPattern(::pattern, ::converter) }
        
        tester.parse("1 2").isWip(1).isOk(1).isDone().isValue(1 astAdd 2).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing simple, then correct error`()
    {
        val tester = Tester { ParserPattern(::pattern, ::converter) }
        
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("1").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("1 true").isWip(1).isBad { ParseError.UnexpectedToken(it[1]) }
    }
}
