package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import org.junit.jupiter.api.*

/**
 * Determines whether the current token stream is parsed correctly. The expectation is that there will be [wipCount]
 * number of tokens resulting in [ParseOk.Incomplete], followed by [postOkCount] [ParseOk.Complete], before finally
 * resulting in [ParseOk.Finished].
 */
private fun <Type> Tester<Type>.isChain(preOkCount: Int = 0, wipCount: Int = 0, postOkCount: Int = 0): Tester<Type> =
    isOk(preOkCount).isWip(wipCount).isOk(postOkCount).isDone()

class TestParserInstructions
{
    private val tester = Tester { statementParserOf() }
    
    @Test
    fun `Given valid token, when parsing, then correct statement`()
    {
        // Assignments
        tester.parse("a = 1").isChain(1, 1, 1).isValue("a" astAssign 1)
        tester.parse("a += 1").isChain(1, 1, 1).isValue("a" astAssignAdd 1)
        tester.parse("a -= 1").isChain(1, 1, 1).isValue("a" astAssignSub 1)
        tester.parse("a *= 1").isChain(1, 1, 1).isValue("a" astAssignMul 1)
        tester.parse("a %= 1").isChain(1, 1, 1).isValue("a" astAssignMod 1)
        tester.parse("a /= 1").isChain(1, 1, 1).isValue("a" astAssignDiv 1)
        tester.parse("val foo = 0").isChain(0, 3, 1).isValue(astVarOf("foo", 0))
        
        // Control flow
        tester.parse("if 1 a = 2").isWip(2).isOk(1).isWip(1).isOk(1).isDone()
            .isValue(astIfOf(1, success = listOf("a" astAssign 2)))
        tester.parse("if 1 {} else a = 2").isWip(3).isOk(1).isWip(1).isOk(1).isWip(1).isOk(1).isDone()
            .isValue(astIfOf(1, success = emptyList(), failure = listOf("a" astAssign 2)))
        
        tester.parse("for a in b {}").isChain(0, 5, 1).isValue(astForOf("a", "b".astLoad(), emptyList()))
        tester.parse("for a in b 0").isChain(0, 4, 1).isValue(astForOf("a", "b".astLoad(), listOf(0.astEval)))
        tester.parse("while a {}").isChain(0, 3, 1).isValue(astWhileOf("a".astLoad(), emptyList()))
        tester.parse("while a 0").isChain(0, 2, 1).isValue(astWhileOf("a".astLoad(), listOf(0.astEval)))
        
        tester.parse("raise 1").isChain(0, 1, 1).isValue(1.astReturnError)
        tester.parse("return 1").isChain(0, 1, 1).isValue(1.astReturnValue)
        tester.parse("return _").isChain(0, 1, 1).isValue(AstReturn)
        
        // Function call
        tester.parse("a()").isChain(1, 1, 1).isValue(astInvokeOf("a".astLoad().astCall()))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("a =").isOk(1).isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
