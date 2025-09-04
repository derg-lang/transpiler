package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.*
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

class TestParserConstant
{
    private val tester = Tester { constantParserOf() }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        // Type and value must be correctly parsed
        tester.parse("val foo      = 1").isChain(3, 1).isValue(astConstOf("foo", type = null, value = 1))
        tester.parse("val foo: Bar = 2").isChain(5, 1).isValue(astConstOf("foo", type = "Bar", value = 2))
        
        // Visibility must be correctly parsed
        tester.parse("exported  val foo = 0").isChain(4, 1).isValue(astConstOf("foo", vis = Visibility.EXPORTED))
        tester.parse("public    val foo = 0").isChain(4, 1).isValue(astConstOf("foo", vis = Visibility.PUBLIC))
        tester.parse("protected val foo = 0").isChain(4, 1).isValue(astConstOf("foo", vis = Visibility.PROTECTED))
        tester.parse("private   val foo = 0").isChain(4, 1).isValue(astConstOf("foo", vis = Visibility.PRIVATE))
        tester.parse("          val foo = 0").isChain(3, 1).isValue(astConstOf("foo", vis = Visibility.PRIVATE))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("val").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("val foo *").isWip(2).isBad { ParseError.UnexpectedToken(Keyword(Symbol.MULTIPLY)) }
    }
}

class TestParserFunction
{
    private val tester = Tester { functionParserOf() }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        // Basic structure must be correctly parsed
        tester.parse("fun foo() {}").isChain(5, 1).isValue(astFunOf("foo"))
        tester.parse("fun foo() -> Foo {}").isChain(7, 1).isValue(astFunOf("foo", valType = "Foo"))
        tester.parse("fun foo(): Foo {}").isChain(7, 1).isValue(astFunOf("foo", errType = "Foo"))
        tester.parse("fun foo(): Foo -> Bar {}").isChain(9, 1)
            .isValue(astFunOf("foo", valType = "Bar", errType = "Foo"))
        
        // Parameters must be correctly parsed
        tester.parse("fun foo(a: mut Foo) {}").isChain(9, 1)
            .isValue(astFunOf("foo", params = listOf(astParOf("a", type = "Foo", mut = Mutability.MUTABLE))))
        tester.parse("fun foo(a:     Foo) {}").isChain(8, 1)
            .isValue(astFunOf("foo", params = listOf(astParOf("a", type = "Foo", mut = Mutability.IMMUTABLE))))
        
        tester.parse("fun foo(in     a: Foo) {}").isChain(9, 1)
            .isValue(astFunOf("foo", params = listOf(astParOf("a", type = "Foo", pas = Passability.IN))))
        tester.parse("fun foo(out    a: Foo) {}").isChain(9, 1)
            .isValue(astFunOf("foo", params = listOf(astParOf("a", type = "Foo", pas = Passability.OUT))))
        tester.parse("fun foo(move   a: Foo) {}").isChain(9, 1)
            .isValue(astFunOf("foo", params = listOf(astParOf("a", type = "Foo", pas = Passability.MOVE))))
        tester.parse("fun foo(borrow a: Foo) {}").isChain(9, 1)
            .isValue(astFunOf("foo", params = listOf(astParOf("a", type = "Foo", pas = Passability.BORROW))))
        tester.parse("fun foo(       a: Foo) {}").isChain(8, 1)
            .isValue(astFunOf("foo", params = listOf(astParOf("a", type = "Foo", pas = Passability.IN))))
        
        tester.parse("fun foo(a: Foo, b: Bar) {}").isChain(12, 1)
            .isValue(astFunOf("foo", params = listOf(astParOf("a", type = "Foo"), astParOf("b", type = "Bar"))))
        
        // Default values for parameters must be supported
        tester.parse("fun foo(a: Foo = 1) {}").isChain(10, 1)
            .isValue(astFunOf("foo", params = listOf(astParOf("a", type = "Foo", value = 1))))
        
        // Visibility must be correctly parsed
        tester.parse("exported  fun foo() {}").isChain(6, 1).isValue(astFunOf("foo", vis = Visibility.EXPORTED))
        tester.parse("public    fun foo() {}").isChain(6, 1).isValue(astFunOf("foo", vis = Visibility.PUBLIC))
        tester.parse("protected fun foo() {}").isChain(6, 1).isValue(astFunOf("foo", vis = Visibility.PROTECTED))
        tester.parse("private   fun foo() {}").isChain(6, 1).isValue(astFunOf("foo", vis = Visibility.PRIVATE))
        tester.parse("          fun foo() {}").isChain(5, 1).isValue(astFunOf("foo", vis = Visibility.PRIVATE))
    }
    
    @Test
    fun `Given return statement, when parsing, then correctly parsed`()
    {
        tester.parse("fun f() { return _ }").isChain(7, 1)
            .isValue(astFunOf("f", valType = null, statements = listOf(AstReturn)))
        tester.parse("fun f() -> Int { return 0 }").isChain(9, 1)
            .isValue(astFunOf("f", valType = "Int", statements = listOf(0.astReturnValue)))
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
        val expected = astFunOf("foo", statements = listOf(astVarOf("bar", 0)))
        
        tester.parse("fun foo() { val bar = 0 }").isChain(9, 1).isValue(expected)
    }
}

class TestParserStruct
{
    private val tester = Tester { structParserOf() }
    
    @Test
    fun `Given valid segment, when parsing, then correctly parsed`()
    {
        // Basic structure must be correctly parsed
        tester.parse("struct Foo {}").isChain(3, 1).isValue(astStructOf("Foo"))
        
        // Properties must be correctly parsed
        tester.parse("struct Foo { val a: Bar }").isChain(7, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", ass = Assignability.FINAL))))
        tester.parse("struct Foo { var a: Bar }").isChain(7, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", ass = Assignability.ASSIGNABLE))))
        tester.parse("struct Foo { ref a: Bar }").isChain(7, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", ass = Assignability.REFERENCE))))
        
        tester.parse("struct Foo { val a: mut Bar }").isChain(8, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", mut = Mutability.MUTABLE))))
        tester.parse("struct Foo { val a:     Bar }").isChain(7, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", mut = Mutability.IMMUTABLE))))
        
        tester.parse("struct Foo { exported  val a: Bar }").isChain(8, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", vis = Visibility.EXPORTED))))
        tester.parse("struct Foo { public    val a: Bar }").isChain(8, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", vis = Visibility.PUBLIC))))
        tester.parse("struct Foo { protected val a: Bar }").isChain(8, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", vis = Visibility.PROTECTED))))
        tester.parse("struct Foo { private   val a: Bar }").isChain(8, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", vis = Visibility.PRIVATE))))
        tester.parse("struct Foo {           val a: Bar }").isChain(7, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", vis = Visibility.PRIVATE))))
        
        tester.parse("struct Foo { val a: Bar val b: Baz }").isChain(11, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar"), astPropOf("b", type = "Baz"))))
        
        // Default values for properties must be supported
        tester.parse("struct Foo { val a: Bar = 1 }").isChain(9, 1)
            .isValue(astStructOf("Foo", props = listOf(astPropOf("a", type = "Bar", value = 1))))
        
        // Visibility must be correctly parsed
        tester.parse("exported  struct Foo {}").isChain(4, 1).isValue(astStructOf("Foo", vis = Visibility.EXPORTED))
        tester.parse("public    struct Foo {}").isChain(4, 1).isValue(astStructOf("Foo", vis = Visibility.PUBLIC))
        tester.parse("protected struct Foo {}").isChain(4, 1).isValue(astStructOf("Foo", vis = Visibility.PROTECTED))
        tester.parse("private   struct Foo {}").isChain(4, 1).isValue(astStructOf("Foo", vis = Visibility.PRIVATE))
        tester.parse("          struct Foo {}").isChain(3, 1).isValue(astStructOf("Foo", vis = Visibility.PRIVATE))
    }
    
    @Test
    fun `Given template, when parsing, then correctly parsed`()
    {
        val generics = listOf(
            astTemplateStruct(name = "Bar"),
            astTemplateValue(name = "baz", type = "Baz".astLoad().astType(), default = 42.ast),
        )
        val expected = astStructOf(name = "Foo", templates = generics)
        
        tester.parse("struct Foo[Bar, baz: Baz = 42] {}").isChain(12, 1).isValue(expected)
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("struct").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("struct Foo {").isWip(3).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserVariable
{
    private val tester = Tester { variableParserOf() }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        // Assignability must be correctly parsed
        tester.parse("val foo = 0").isChain(3, 1).isValue(astVarOf("foo", 0, ass = Assignability.FINAL))
        tester.parse("var foo = 0").isChain(3, 1).isValue(astVarOf("foo", 0, ass = Assignability.ASSIGNABLE))
        tester.parse("ref foo = 0").isChain(3, 1).isValue(astVarOf("foo", 0, ass = Assignability.REFERENCE))
        
        // Mutability must be correctly parsed
        tester.parse("val foo: mut Int = 0").isChain(6, 1).isValue(astVarOf("foo", 0, type = "Int", mut = Mutability.MUTABLE))
        tester.parse("val foo:     Int = 0").isChain(5, 1).isValue(astVarOf("foo", 0, type = "Int", mut = Mutability.IMMUTABLE))
        
        // Visibility must be correctly parsed
        tester.parse("exported  val foo = 0").isChain(4, 1).isValue(astVarOf("foo", 0, vis = Visibility.EXPORTED))
        tester.parse("public    val foo = 0").isChain(4, 1).isValue(astVarOf("foo", 0, vis = Visibility.PUBLIC))
        tester.parse("protected val foo = 0").isChain(4, 1).isValue(astVarOf("foo", 0, vis = Visibility.PROTECTED))
        tester.parse("private   val foo = 0").isChain(4, 1).isValue(astVarOf("foo", 0, vis = Visibility.PRIVATE))
        tester.parse("          val foo = 0").isChain(3, 1).isValue(astVarOf("foo", 0, vis = Visibility.PRIVATE))
        
        // Types must be correctly parsed
        tester.parse("val foo: Bar = 0").isChain(5, 1).isValue(astVarOf("foo", 0, type = "Bar"))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("val").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("val foo =").isWip(3).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
