package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

private const val REAL = "real"
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
        val tester = Tester { ParserAnyOf(ParserReal(), ParserBool()) }
        
        tester.parse("42").isOk(1).isDone().isValue(42.toExp()).resets()
        tester.parse("true").isOk(1).isDone().isValue(true.toExp()).resets()
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserAnyOf(
                ParserSequence(REAL to ParserReal(), BOOL to ParserBool()),
                ParserSequence(TEXT to ParserText(), BOOL to ParserBool()),
                ParserSequence(TEXT to ParserText()),
            )
        }
        
        tester.parse("42 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 42.toExp()).hasValue(BOOL, true.toExp()).resets()
        tester.parse("\"foo\" false").isOk(2).isDone()
            .hasValue(TEXT, "foo".toExp()).hasValue(BOOL, false.toExp()).resets()
        tester.parse("\"foo\"").isOk(1).isDone()
            .hasValue(TEXT, "foo".toExp()).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserAnyOf(ParserReal(), ParserBool()) }
        
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
        val tester = Tester { ParserAllOf(REAL to ParserReal(), BOOL to ParserBool()) }
        
        tester.parse("42 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 42.toExp()).hasValue(BOOL, true.toExp())
        tester.parse("true 42").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 42.toExp()).hasValue(BOOL, true.toExp())
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserAllOf(
                REAL to ParserSequence("a" to ParserReal(), "b" to ParserReal()),
                BOOL to ParserBool(),
                TEXT to ParserOptional(ParserText()),
            )
        }
        
        tester.parse("1 2 true").isWip(2).isOk(1).isDone()
            .hasValue(REAL, "a" to 1.toExp(), "b" to 2.toExp())
            .hasValue(BOOL, true.toExp())
        tester.parse("\"foo\" 1 2 true").isWip(3).isOk(1).isDone()
            .hasValue(REAL, "a" to 1.toExp(), "b" to 2.toExp())
            .hasValue(BOOL, true.toExp())
            .hasValue(TEXT, "foo".toExp())
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserAllOf(REAL to ParserReal(), BOOL to ParserBool()) }
        
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
                REAL to ParserReal(),
                BOOL to ParserBool(),
                TEXT to ParserText(),
            )
        }
        
        tester.parse("1 true \"foo\"").isWip(2).isOk(1).isDone()
            .hasValue(REAL, 1.toExp()).hasValue(BOOL, true.toExp()).hasValue(TEXT, "foo".toExp())
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserSequence(
                REAL to ParserSequence("a" to ParserReal(), "b" to ParserReal()),
                BOOL to ParserSequence("a" to ParserBool(), "b" to ParserBool()),
                TEXT to ParserOptional(ParserText()),
            )
        }
        
        tester.parse("1 2 true false").isWip(3).isOk(1).isDone()
            .hasValue(REAL, "a" to 1.toExp(), "b" to 2.toExp())
            .hasValue(BOOL, "a" to true.toExp(), "b" to false.toExp())
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserSequence(REAL to ParserReal(), BOOL to ParserBool()) }
        
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
        tester.parse("1").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserRepeating
{
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserRepeating(ParserReal(), ParserSymbol(SymbolType.COMMA)) }
        
        tester.parse("").isDone().isValue(emptyList())
        tester.parse("1").isOk(1).isDone().isValue(listOf(1.toExp())).resets()
        tester.parse("1,").isOk(2).isDone().isValue(listOf(1.toExp())).resets()
        tester.parse("1,2").isOk(3).isDone().isValue(listOf(1.toExp(), 2.toExp())).resets()
        tester.parse("1,2,").isOk(4).isDone().isValue(listOf(1.toExp(), 2.toExp())).resets()
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserRepeating(
                ParserSequence(REAL to ParserReal(), BOOL to ParserBool()),
                ParserSymbol(SymbolType.COMMA),
            )
        }
        
        tester.parse("1 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 1.toExp()).hasValue(BOOL, true.toExp())
        tester.parse("1 true,").isWip(1).isOk(2).isDone()
            .hasValue(REAL, 1.toExp()).hasValue(BOOL, true.toExp())
        tester.parse("1 true, 2 false").isWip(1).isOk(2).isWip(1).isOk(1).isDone()
            .hasValue(REAL, 1.toExp(), 2.toExp()).hasValue(BOOL, true.toExp(), false.toExp())
        tester.parse("1 true, 2 false,").isWip(1).isOk(2).isWip(1).isOk(2).isDone()
            .hasValue(REAL, 1.toExp(), 2.toExp()).hasValue(BOOL, true.toExp(), false.toExp())
    }
    
    @Test
    fun `Given valid token, when parsing empty separator, then value produced`()
    {
        val tester = Tester { ParserRepeating(ParserSequence(REAL to ParserReal(), BOOL to ParserBool())) }
        
        tester.parse("1 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 1.toExp())
        tester.parse("1 true 2 false").isWip(1).isOk(1).isWip(1).isOk(1).isDone()
            .hasValue(REAL, 1.toExp(), 2.toExp()).hasValue(BOOL, true.toExp(), false.toExp())
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester()
        {
            ParserRepeating(
                ParserSequence(REAL to ParserReal(), BOOL to ParserBool()),
                ParserSymbol(SymbolType.COMMA),
            )
        }
        
        tester.parse("1").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("1 true, 2").isWip(1).isOk(2).isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
    
    @Test
    fun `Given produced values, when resetting, then values are retained`()
    {
        val parser = ParserRepeating(ParserReal())
            .also { it.parse(Numeric(1, null)) }
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
        val tester = Tester { ParserOptional(ParserReal()) }
        
        tester.parse("").isDone().isValue(null).resets()
        tester.parse("42").isOk(1).isDone().isValue(42.toExp()).resets()
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester { ParserOptional(ParserSequence(REAL to ParserReal(), BOOL to ParserBool())) }
        
        tester.parse("").isDone()
            .hasValue(REAL, null).hasValue(BOOL, null)
        tester.parse("42 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 42.toExp()).hasValue(BOOL, true.toExp())
    }
    
    @Test
    fun `Given missing token, when parsing simple, then default produced`()
    {
        val tester = Tester { ParserOptional(ParserReal(), 7.toExp()) }
        
        tester.parse("").isDone().isValue(7.toExp()).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserOptional(ParserSequence(REAL to ParserReal(), BOOL to ParserBool())) }
        
        tester.parse("1 bah").isWip(1).isBad { ParseError.UnexpectedToken(it[1]) }
    }
}

class TestParserPattern
{
    private fun pattern() = ParserSequence("lhs" to ParserReal(), "rhs" to ParserReal())
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
