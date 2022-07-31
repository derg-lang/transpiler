package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Assignment
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.ParserTester
import com.github.derg.transpiler.parser.toLit
import org.junit.jupiter.api.Test

class TestParserAssignment
{
    private val tester = ParserTester { ParserAssignment }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        tester.parse("a = 1").isGood(3, Assignment.Assign("a", 1.toLit()))
        tester.parse("a += 1").isGood(3, Assignment.AssignAdd("a", 1.toLit()))
        tester.parse("a -= 1").isGood(3, Assignment.AssignSubtract("a", 1.toLit()))
        tester.parse("a *= 1").isGood(3, Assignment.AssignMultiply("a", 1.toLit()))
        tester.parse("a /= 1").isGood(3, Assignment.AssignDivide("a", 1.toLit()))
        tester.parse("a %= 1").isGood(3, Assignment.AssignModulo("a", 1.toLit()))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.End }
        tester.parse("foo").isBad { ParseError.End }
        tester.parse("foo = ").isBad { ParseError.End }
        tester.parse("if").isBad { ParseError.NotIdentifier(it[0]) }
    }
}
