package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.Mutability
import com.github.derg.transpiler.source.Visibility
import com.github.derg.transpiler.source.lexeme.EndOfFile
import org.junit.jupiter.api.Test

/**
 * Determines whether the current token stream is parsed correctly. The expectation is that there will be [wipCount]
 * number of tokens resulting in [ParseOk.Incomplete], followed by [postOkCount] [ParseOk.Complete], before finally
 * resulting in [ParseOk.Finished].
 */
private fun <Type> Tester<Type>.isChain(wipCount: Int = 0, postOkCount: Int = 0): Tester<Type> =
    isWip(wipCount).isOk(postOkCount).isDone()

class TestParserVariable
{
    private val tester = Tester { variableParserOf() }
    
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

class TestParserFunction
{
    private val tester = Tester { functionParserOf() }
    
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
        
        tester.parse("fun foo(val a: Foo, val b: Bar) {}").isChain(14, 1)
            .isValue(funOf("foo", params = listOf(parOf("a", type = "Foo"), parOf("b", type = "Bar"))))
        
        // Default values for parameters must be supported
        tester.parse("fun foo(val a = 1) {}").isChain(9, 1)
            .isValue(funOf("foo", params = listOf(parOf("a", value = 1))))
        tester.parse("fun foo(val a: Foo = 1) {}").isChain(11, 1)
            .isValue(funOf("foo", params = listOf(parOf("a", type = "Foo", value = 1))))
        
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
    
    @Test
    fun `Given function with variable, when parsing, then correctly parsed`()
    {
        val expected = funOf("foo", scope = scopeOf(true, varOf("bar", 0)))
        
        tester.parse("fun foo() { val bar = 0 }").isChain(9, 1).isValue(expected)
    }
}

class TestParserSegment
{
    private val tester = Tester { segmentParserOf() }
    
    @Test
    fun `Given valid segment, when parsing, then correctly parsed`()
    {
        tester.parse("").isChain().isValue(segmentOf())
        tester.parse("module foo").isChain(1, 1).isValue(segmentOf(module = "foo"))
        tester.parse("use foo").isChain(1, 1).isValue(segmentOf(imports = setOf("foo")))
        tester.parse("val foo = 0").isChain(3, 1).isValue(segmentOf(statements = listOf(varOf("foo", 0))))
        tester.parse("fun foo() {}").isChain(5, 1).isValue(segmentOf(statements = listOf(funOf("foo"))))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("module").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("use").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserType
{
    private val tester = Tester { typeParserOf() }
    
    @Test
    fun `Given valid segment, when parsing, then correctly parsed`()
    {
        // Basic structure must be correctly parsed
        tester.parse("type Foo {}").isChain(3, 1).isValue(typeOf("Foo"))
        
        // Properties must be correctly parsed
        tester.parse("type Foo { val a: Bar }").isChain(7, 1)
            .isValue(typeOf("Foo", props = listOf(propOf("a", type = "Bar", mut = Mutability.VALUE))))
        tester.parse("type Foo { var a: Bar }").isChain(7, 1)
            .isValue(typeOf("Foo", props = listOf(propOf("a", type = "Bar", mut = Mutability.VARYING))))
        tester.parse("type Foo { mut a: Bar }").isChain(7, 1)
            .isValue(typeOf("Foo", props = listOf(propOf("a", type = "Bar", mut = Mutability.MUTABLE))))
        
        tester.parse("type Foo { pub val a: Bar }").isChain(8, 1)
            .isValue(typeOf("Foo", props = listOf(propOf("a", type = "Bar", vis = Visibility.PUBLIC))))
        tester.parse("type Foo {     val a: Bar }").isChain(7, 1)
            .isValue(typeOf("Foo", props = listOf(propOf("a", type = "Bar", vis = Visibility.PRIVATE))))
        
        tester.parse("type Foo { val a: Foo val b: Bar }").isChain(11, 1)
            .isValue(typeOf("Foo", props = listOf(propOf("a", type = "Foo"), propOf("b", type = "Bar"))))
        
        // Default values for properties must be supported
        tester.parse("type Foo { val a = 1 }").isChain(7, 1)
            .isValue(typeOf("Foo", props = listOf(propOf("a", value = 1))))
        
        // Visibility must be correctly parsed
        tester.parse("pub type Foo {}").isChain(4, 1).isValue(typeOf("Foo", vis = Visibility.PUBLIC))
        tester.parse("    type Foo {}").isChain(3, 1).isValue(typeOf("Foo", vis = Visibility.PRIVATE))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("type").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("type Foo {").isWip(3).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
