package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import org.junit.jupiter.api.*

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
        tester.parse("a = 1").isChain(2, 1).isValue("a" astAssign 1)
        tester.parse("a += 1").isChain(2, 1).isValue("a" astAssignAdd 1)
        tester.parse("a -= 1").isChain(2, 1).isValue("a" astAssignSub 1)
        tester.parse("a *= 1").isChain(2, 1).isValue("a" astAssignMul 1)
        tester.parse("a %= 1").isChain(2, 1).isValue("a" astAssignMod 1)
        tester.parse("a /= 1").isChain(2, 1).isValue("a" astAssignDiv 1)
        tester.parse("val foo = 0").isChain(3, 1).isValue(astVarOf("foo", 0))
        
        // Control flow
        tester.parse("if 1 a = 2").isWip(4).isOk(1).isDone()
            .isValue(astIfOf(1, success = listOf("a" astAssign 2)))
        tester.parse("if 1 {} else a = 2").isWip(3).isOk(1).isWip(3).isOk(1).isDone()
            .isValue(astIfOf(1, success = emptyList(), failure = listOf("a" astAssign 2)))
        
        tester.parse("raise 1").isChain(1, 1).isValue(1.astReturnError)
        tester.parse("return 1").isChain(1, 1).isValue(1.astReturnValue)
        tester.parse("return _").isChain(1, 1).isValue(AstReturn)
        
        // Function call
        tester.parse("a()").isChain(2, 1).isValue(astInvokeOf("a".astLoad().astCall()))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("a =").isWip(2).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
