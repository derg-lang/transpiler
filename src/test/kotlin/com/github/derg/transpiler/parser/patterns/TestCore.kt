package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.lexer.Structure
import com.github.derg.transpiler.parser.ParseError.End
import com.github.derg.transpiler.parser.ParseError.NotExpression
import com.github.derg.transpiler.parser.PatternTester
import com.github.derg.transpiler.parser.value
import org.junit.jupiter.api.Test

class TestParserAnyOf
{
    private val real = ParserRealExpression
    private val text = ParserTextExpression
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        val tester = PatternTester { ParserAnyOf(text, real) }
        
        tester.parse("7").isGood(1, 7.value)
        tester.parse("\"foo\"").isGood(1, "foo".value)
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = PatternTester { ParserAnyOf(text, real) }
        
        tester.parse("").isBad { End }
        tester.parse("false").isBad { NotExpression(it[0]) }
    }
}

class TestParserSequence
{
    private val tester = PatternTester { ParserSequence(ParserRealExpression, ParserBoolExpression) }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        tester.parse("3 true").isGood(2, listOf(3.value, true.value))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { End }
        tester.parse("3").isBad { End }
        tester.parse("false").isBad { NotExpression(it[0]) }
        tester.parse("2 if").isBad { NotExpression(it[1]) }
    }
}

class TestParserRepeating
{
    private val tester = PatternTester { ParserRepeating(ParserRealExpression, ParserStructure(Structure.Type.COMMA)) }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        tester.parse("").isGood(0, emptyList())
        tester.parse("1").isGood(1, listOf(1.value))
        tester.parse("1, 2").isGood(3, listOf(1.value, 2.value))
        tester.parse("1, 2, ").isGood(4, listOf(1.value, 2.value))
        tester.parse("if").isGood(0, emptyList())
    }
}

class TestParserOptional
{
    private val tester = PatternTester { ParserOptional(ParserRealExpression) }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        tester.parse("").isGood(0, null)
        tester.parse("if").isGood(0, null)
        tester.parse("7").isGood(1, 7.value)
    }
}
