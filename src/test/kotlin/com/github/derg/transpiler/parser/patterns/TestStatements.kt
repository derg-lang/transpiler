package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.lexer.EndOfFile
import com.github.derg.transpiler.parser.*
import org.junit.jupiter.api.Test

class TestParserStatement
{
    private val tester = Tester { ParserStatement() }
    
    @Test
    fun `Given valid token, when parsing, then correct statement`()
    {
        // Assignments
        tester.parse("a = 1").isWip(2).isOk(1).isDone().isValue("a" assign 1)
        tester.parse("a += 1").isWip(2).isOk(1).isDone().isValue("a" assignAdd 1)
        tester.parse("a -= 1").isWip(2).isOk(1).isDone().isValue("a" assignSub 1)
        tester.parse("a *= 1").isWip(2).isOk(1).isDone().isValue("a" assignMul 1)
        tester.parse("a %= 1").isWip(2).isOk(1).isDone().isValue("a" assignMod 1)
        tester.parse("a /= 1").isWip(2).isOk(1).isDone().isValue("a" assignDiv 1)
        
        // Control flow
        tester.parse("if 1 a = 2").isWip(4).isOk(1).isDone()
            .isValue(branchOf(1, scopeOf(isBraced = false, "a" assign 2)))
        tester.parse("if 1 {} else a = 2").isWip(3).isOk(1).isWip(3).isOk(1).isDone()
            .isValue(branchOf(1, scopeOf(isBraced = true), scopeOf(isBraced = false, "a" assign 2)))
    
        tester.parse("raise 1").isWip(1).isOk(1).isDone().isValue(raiseOf(1))
        tester.parse("return 1").isWip(1).isOk(1).isDone().isValue(returnOf(1))
        tester.parse("return _").isWip(1).isOk(1).isDone().isValue(returnOf())
        
        // Function call
        tester.parse("a()").isWip(2).isOk(1).isDone().isValue(callOf("a".toFun()))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("a =").isWip(2).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserScope
{
    private val tester = Tester { ParserScope() }
    
    @Test
    fun `Given valid token, when parsing, then correct scope`()
    {
        tester.parse("{}").isWip(1).isOk(1).isDone()
            .isValue(scopeOf(isBraced = true))
        tester.parse("a = 1").isWip(2).isOk(1).isDone()
            .isValue(scopeOf(isBraced = false, "a" assign 1))
        tester.parse("{ a = 1 }").isWip(4).isOk(1).isDone()
            .isValue(scopeOf(isBraced = true, "a" assign 1))
        tester.parse("{ a = 1 b = 2 }").isWip(7).isOk(1).isDone()
            .isValue(scopeOf(isBraced = true, "a" assign 1, "b" assign 2))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("a").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("{").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
