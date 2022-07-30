package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.lexer.Operator.Type.MINUS
import com.github.derg.transpiler.lexer.Operator.Type.MULTIPLY
import com.github.derg.transpiler.lexer.Structure.Type.COMMA
import com.github.derg.transpiler.lexer.Structure.Type.OPEN_BRACKET
import com.github.derg.transpiler.parser.ParseError.*
import com.github.derg.transpiler.parser.ParserTester
import org.junit.jupiter.api.Test

class TestParserIdentifier
{
    private val tester = ParserTester { ParserIdentifier }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        tester.parse("test").isGood(1, "test")
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { End }
        tester.parse("false").isBad { NotIdentifier(it[0]) }
    }
}

class TestParserOperator
{
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        val tester = ParserTester { ParserOperator() }
        
        tester.parse("*").isGood(1, MULTIPLY)
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = ParserTester { ParserOperator(MINUS) }
        
        tester.parse("").isBad { End }
        tester.parse("*").isBad { WrongOperator(setOf(MINUS), MULTIPLY) }
        tester.parse("false").isBad { NotOperator(it[0]) }
    }
}

class TestParserStructure
{
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        val tester = ParserTester { ParserStructure() }
        
        tester.parse(",").isGood(1, COMMA)
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = ParserTester { ParserStructure(COMMA) }
        
        tester.parse("").isBad { End }
        tester.parse("[").isBad { WrongStructure(setOf(COMMA), OPEN_BRACKET) }
        tester.parse("false").isBad { NotStructure(it[0]) }
    }
}
