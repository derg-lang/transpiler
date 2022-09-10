package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Mutability
import com.github.derg.transpiler.ast.Visibility
import com.github.derg.transpiler.lexer.EndOfFile
import com.github.derg.transpiler.parser.*
import org.junit.jupiter.api.Test

/**
 * Determines whether the current token stream is parsed correctly. The expectation is that there will be [preOkCount]
 * number of tokens resulting in [ParseOk.Complete], followed by [wipCount] [ParseOk.Incomplete], then followed by
 * [postOkCount] [ParseOk.Complete] again, before finally resulting in [ParseOk.Finished].
 */
private fun <Type> Tester<Type>.isChain(wipCount: Int = 0, postOkCount: Int = 0): Tester<Type> =
    isWip(wipCount).isOk(postOkCount).isDone()

class TestParserVariableDefinition
{
    private val tester = Tester { ParserVariableDefinition() }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        // Mutability must be correctly parsed
        tester.parse("val foo = 0").isChain(3, 1).isValue(varOf("foo", 0, mut = Mutability.VALUE))
        tester.parse("var foo = 0").isChain(3, 1).isValue(varOf("foo", 0, mut = Mutability.VARYING))
        tester.parse("mut foo = 0").isChain(3, 1).isValue(varOf("foo", 0, mut = Mutability.MUTABLE))
        
        // Visibility must be correctly parsed
        tester.parse("pub val foo = 0").isChain(4, 1).isValue(varOf("foo", 0, vis = Visibility.PUBLIC))
        tester.parse("    val foo = 0").isChain(3, 1).isValue(varOf("foo", 0, vis = Visibility.PRIVATE))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("val").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("val foo =").isWip(3).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserFunctionDefinition
{
    private val tester = Tester { ParserFunctionDefinition() }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        // Basic structure must be correctly parsed
        tester.parse("fun foo() {}").isChain(5, 1).isValue(funOf("foo"))
        tester.parse("fun foo() -> Foo {}").isChain(7, 1).isValue(funOf("foo", valType = "Foo"))
        tester.parse("fun foo(): Foo {}").isChain(7, 1).isValue(funOf("foo", errType = "Foo"))
        tester.parse("fun foo(): Foo -> Bar {}").isChain(9, 1).isValue(funOf("foo", valType = "Bar", errType = "Foo"))
        
        // Parameters must be correctly parsed
        tester.parse("fun foo(val a: Foo) {}").isChain(9, 1)
            .isValue(funOf("foo", params = listOf(parOf("a", type = "Foo", mut = Mutability.VALUE))))
        tester.parse("fun foo(var a: Foo) {}").isChain(9, 1)
            .isValue(funOf("foo", params = listOf(parOf("a", type = "Foo", mut = Mutability.VARYING))))
        tester.parse("fun foo(mut a: Foo) {}").isChain(9, 1)
            .isValue(funOf("foo", params = listOf(parOf("a", type = "Foo", mut = Mutability.MUTABLE))))
        
        val params = listOf(
            parOf("a", type = "Foo", mut = Mutability.VALUE),
            parOf("b", type = "Bar", mut = Mutability.VALUE),
        )
        
        tester.parse("fun foo(val a: Foo, val b: Bar) {}").isChain(14, 1)
            .isValue(funOf("foo", params = params))
        
        // Visibility must be correctly parsed
        tester.parse("pub fun foo() {}").isChain(6, 1).isValue(funOf("foo", vis = Visibility.PUBLIC))
        tester.parse("    fun foo() {}").isChain(5, 1).isValue(funOf("foo", vis = Visibility.PRIVATE))
    }
    
    @Test
    fun `Given return statement, when parsing, then correctly parsed`()
    {
        tester.parse("fun f() { return _ }").isChain(7, 1)
            .isValue(funOf("f", valType = null, scope = scopeOf(true, returnOf())))
        tester.parse("fun f() -> Int { return 0 }").isChain(9, 1)
            .isValue(funOf("f", valType = "Int", scope = scopeOf(true, returnOf(0))))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("fun").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("fun foo(").isWip(3).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
