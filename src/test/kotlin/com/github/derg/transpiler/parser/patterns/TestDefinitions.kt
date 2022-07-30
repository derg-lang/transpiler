package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.ParserTester
import com.github.derg.transpiler.parser.variableOf
import org.junit.jupiter.api.Test

class TestParseVariableDefinition
{
    private val tester = ParserTester { ParserVariableDefinition }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        tester.parse("true").isGood(1, variableOf("foo"))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.End }
        tester.parse("if").isBad { ParseError.NotExpression(it[0]) }
    }
}
