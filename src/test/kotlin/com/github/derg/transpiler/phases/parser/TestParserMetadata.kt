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

class TestParserScope
{
    private val tester = Tester { scopeParserOf() }
    
    @Test
    fun `Given valid token, when parsing, then correct scope`()
    {
        tester.parse("{}").isChain(1, 1).isValue(emptyList())
        tester.parse("a = 1").isChain(2, 1).isValue(listOf("a" astAssign 1))
        tester.parse("{ a = 1 }").isChain(4, 1).isValue(listOf("a" astAssign 1))
        tester.parse("{ a = 1 b = 2 }").isChain(7, 1).isValue(listOf("a" astAssign 1, "b" astAssign 2))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("a").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("{").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserSegment
{
    private val tester = Tester { segmentParserOf() }
    
    @Test
    fun `Given valid segment, when parsing, then correctly parsed`()
    {
        tester.parse("").isDone().isValue(astSegmentOf())
        tester.parse("module foo").isWip(2).isDone().isValue(astSegmentOf(module = "foo"))
        tester.parse("use foo").isWip(2).isDone().isValue(astSegmentOf(imports = listOf("foo")))
        tester.parse("val foo = 0").isWip(4).isDone().isValue(astSegmentOf(statements = listOf(astVarOf("foo", 0))))
        tester.parse("fun foo() {}").isWip(6).isDone().isValue(astSegmentOf(statements = listOf(astFunOf("foo"))))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("module").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("use").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
