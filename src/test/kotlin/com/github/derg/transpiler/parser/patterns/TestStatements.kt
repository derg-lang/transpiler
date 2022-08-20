package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.lexer.EndOfFile
import com.github.derg.transpiler.parser.*
import org.junit.jupiter.api.Test

class TestParserAssignment
{
    private val tester = Tester { ParserAssignment() }
    
    @Test
    fun `Given valid token, when parsing, then correct assignment`()
    {
        tester.parse("a = 1").isWip(2).isOk(1).isDone().isValue("a" assign 1)
        tester.parse("a += 1").isWip(2).isOk(1).isDone().isValue("a" assignAdd 1)
        tester.parse("a -= 1").isWip(2).isOk(1).isDone().isValue("a" assignSub 1)
        tester.parse("a *= 1").isWip(2).isOk(1).isDone().isValue("a" assignMul 1)
        tester.parse("a %= 1").isWip(2).isOk(1).isDone().isValue("a" assignMod 1)
        tester.parse("a /= 1").isWip(2).isOk(1).isDone().isValue("a" assignDiv 1)
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("a").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("a =").isWip(2).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
