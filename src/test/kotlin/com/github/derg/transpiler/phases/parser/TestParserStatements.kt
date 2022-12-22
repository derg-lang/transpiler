package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.lexeme.EndOfFile
import org.junit.jupiter.api.Test

/**
 * Determines whether the current token stream is parsed correctly. The expectation is that there will be [wipCount]
 * number of tokens resulting in [ParseOk.Incomplete], followed by [postOkCount] [ParseOk.Complete], before finally
 * resulting in [ParseOk.Finished].
 */
private fun <Type> Tester<Type>.isChain(wipCount: Int = 0, postOkCount: Int = 0): Tester<Type> =
    isWip(wipCount).isOk(postOkCount).isDone()

class TestParserStatement
{
    private val tester = Tester { statementParserOf() }
    
    @Test
    fun `Given valid token, when parsing, then correct statement`()
    {
        // Assignments
        tester.parse("a = 1").isChain(2, 1).isValue("a" assign 1)
        tester.parse("a += 1").isChain(2, 1).isValue("a" assignAdd 1)
        tester.parse("a -= 1").isChain(2, 1).isValue("a" assignSub 1)
        tester.parse("a *= 1").isChain(2, 1).isValue("a" assignMul 1)
        tester.parse("a %= 1").isChain(2, 1).isValue("a" assignMod 1)
        tester.parse("a /= 1").isChain(2, 1).isValue("a" assignDiv 1)
        
        // Control flow
        tester.parse("if 1 a = 2").isWip(4).isOk(1).isDone()
            .isValue(ifOf(1, scopeOf(isBraced = false, "a" assign 2)))
        tester.parse("if 1 {} else a = 2").isWip(3).isOk(1).isWip(3).isOk(1).isDone()
            .isValue(ifOf(1, scopeOf(isBraced = true), scopeOf(isBraced = false, "a" assign 2)))
        
        tester.parse("raise 1").isChain(1, 1).isValue(raiseOf(1))
        tester.parse("return 1").isChain(1, 1).isValue(returnOf(1))
        tester.parse("return _").isChain(1, 1).isValue(returnOf())
        
        // Function call
        tester.parse("a()").isChain(2, 1).isValue(callOf("a".toFun()))
        
        // Definitions
        tester.parse("val foo = 0").isChain(3, 1).isValue(varOf("foo", 0))
        tester.parse("fun foo() {}").isChain(5, 1).isValue(funOf("foo"))
        tester.parse("type Foo {}").isChain(3, 1).isValue(typeOf("Foo"))
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
    private val tester = Tester { scopeParserOf() }
    
    @Test
    fun `Given valid token, when parsing, then correct scope`()
    {
        tester.parse("{}").isChain(1, 1).isValue(scopeOf(isBraced = true))
        tester.parse("a = 1").isChain(2, 1).isValue(scopeOf(isBraced = false, "a" assign 1))
        tester.parse("{ a = 1 }").isChain(4, 1).isValue(scopeOf(isBraced = true, "a" assign 1))
        tester.parse("{ a = 1 b = 2 }").isChain(7, 1).isValue(scopeOf(isBraced = true, "a" assign 1, "b" assign 2))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("a").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("{").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
