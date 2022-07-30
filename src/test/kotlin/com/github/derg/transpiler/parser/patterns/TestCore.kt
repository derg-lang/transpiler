package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.lexer.Structure
import com.github.derg.transpiler.parser.ParseError.End
import com.github.derg.transpiler.parser.ParseError.NotExpression
import com.github.derg.transpiler.parser.ParserTester
import com.github.derg.transpiler.parser.toLit
import org.junit.jupiter.api.Test

class TestParserAnyOf
{
    private val real = ParserRealExpression
    private val text = ParserTextExpression
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        val tester = ParserTester { ParserAnyOf(text, real) }
        
        tester.parse("7").isGood(1, 7.toLit())
        tester.parse("\"foo\"").isGood(1, "foo".toLit())
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = ParserTester { ParserAnyOf(text, real) }
        
        tester.parse("").isBad { End }
        tester.parse("false").isBad { NotExpression(it[0]) }
    }
}

class TestParserAllOf
{
    private val real = ParserRealExpression
    private val bool = ParserBoolExpression
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        val tester = ParserTester { ParserAllOf("real" to real, "bool" to bool) }
        val expected = mapOf("real" to 3.toLit(), "bool" to true.toLit())
        
        tester.parse("3 true").isGood(2, expected)
        tester.parse("true 3").isGood(2, expected)
    }
    
    @Test
    fun `Given valid token, when parsing with optionals, then correctly parsed`()
    {
        val tester = ParserTester { ParserAllOf("real" to real, "bool" to ParserOptional(bool)) }
        
        tester.parse("3").isGood(1, mapOf("real" to 3.toLit(), "bool" to null))
        tester.parse("true 3").isGood(2, mapOf("real" to 3.toLit(), "bool" to true.toLit()))
        tester.parse("3 true").isGood(2, mapOf("real" to 3.toLit(), "bool" to true.toLit()))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = ParserTester { ParserAllOf("real" to real, "bool" to bool) }
        
        tester.parse("").isBad { End }
        tester.parse("3").isBad { End }
        tester.parse("false").isBad { End }
        tester.parse("2 if").isBad { NotExpression(it[1]) }
        tester.parse("if").isBad { NotExpression(it[0]) }
    }
}

class TestParserSequence
{
    private val tester = ParserTester { ParserSequence(ParserRealExpression, ParserBoolExpression) }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        tester.parse("3 true").isGood(2, listOf(3.toLit(), true.toLit()))
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
    private val tester = ParserTester { ParserRepeating(ParserRealExpression, ParserStructure(Structure.Type.COMMA)) }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        tester.parse("").isGood(0, emptyList())
        tester.parse("1").isGood(1, listOf(1.toLit()))
        tester.parse("1, 2").isGood(3, listOf(1.toLit(), 2.toLit()))
        tester.parse("1, 2, ").isGood(4, listOf(1.toLit(), 2.toLit()))
        tester.parse("if").isGood(0, emptyList())
    }
}

class TestParserOptional
{
    private val tester = ParserTester { ParserOptional(ParserRealExpression) }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        tester.parse("").isGood(0, null)
        tester.parse("if").isGood(0, null)
        tester.parse("7").isGood(1, 7.toLit())
    }
}
