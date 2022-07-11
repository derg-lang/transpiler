package com.github.derg.transpiler.lexer

import com.github.derg.transpiler.core.Node
import com.github.derg.transpiler.core.NodeAssignment.*
import com.github.derg.transpiler.core.NodeExpression
import com.github.derg.transpiler.core.NodeExpression.*
import com.github.derg.transpiler.core.NodeExpression.Function
import com.github.derg.transpiler.core.ParameterNode
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

private fun Node.asList() = listOf(this)

private val Boolean.e get() = Bool(this)
private val Int.e get() = NodeExpression.Numeric(toBigDecimal(), null)
private val Double.e get() = NodeExpression.Numeric(toBigDecimal(), null)
private val String.e get() = NodeExpression.Textual(this, null)

/**
 * Helper for parsing a [input] source code string into an abstract syntax tree.
 */
private fun parse(input: String): List<Node> = parse(tokenize(input))

class TestParser
{
    @Nested
    inner class Expressions
    {
        @Test
        fun `Given literal, when parsing, then correctly parsed`()
        {
            assertEquals(0.e.asList(), parse("0"))
            assertEquals(1.e.asList(), parse("1"))
            assertEquals(3.14.e.asList(), parse("3.14"))
            assertEquals(true.e.asList(), parse("true"))
            assertEquals(false.e.asList(), parse("false"))
            assertEquals("Hello World".e.asList(), parse(""" "Hello World" """))
            
            assertEquals(NodeExpression.Numeric(0.toBigDecimal(), "s").asList(), parse("0s"))
            assertEquals(NodeExpression.Textual("", "s").asList(), parse("\"\"s"))
        }
        
        @Test
        fun `Given variable, when parsing, then correctly parsed`()
        {
            assertEquals(Variable("foo").asList(), parse("foo"))
        }
        
        @Test
        fun `Given function, when parsing, then correctly parsed`()
        {
            assertEquals(Function("foo", emptyList()).asList(), parse("foo()"))
            assertEquals(Function("f", listOf(ParameterNode(null, Function("b", listOf())))).asList(), parse("f(b())"))
            assertEquals(Function("foo", listOf(ParameterNode(null, 0.e))).asList(), parse("foo(0)"))
            assertEquals(Function("foo", listOf(ParameterNode("bar", 0.e))).asList(), parse("foo(bar = 0)"))
        }
        
        @Test
        fun `Given named function parameters, when parsing, then correctly parsed`()
        {
            val mixedParameters = listOf(
                ParameterNode(null, 1.e),
                ParameterNode("name", 2.e),
                ParameterNode(null, 3.e),
            ).let { Function("foo", it) }
            
            assertEquals(mixedParameters.asList(), parse("foo(1, name = 2, 3)")) // No trailing comma
            assertEquals(mixedParameters.asList(), parse("foo(1, name = 2, 3, )")) // Trailing comma
            
            val sneakyIdentifier = listOf(
                ParameterNode("a", Variable("b")),
                ParameterNode(null, Function("c", emptyList())),
            ).let { Function("bar", it) }
            
            assertEquals(sneakyIdentifier.asList(), parse("bar(a = b, c())"))
        }
        
        @Test
        fun `Given nested functions, when parsing, then correctly parsed`()
        {
            val nestedFunctions = listOf(
                ParameterNode("a", Function("n1", listOf(ParameterNode("inner", 1.e)))),
                ParameterNode("b", Function("n2", listOf(ParameterNode("foo", 2.e), ParameterNode("bar", 3.e)))),
            ).let { Function("nested", it) }
            
            assertEquals(nestedFunctions.asList(), parse("nested(a = n1(inner = 1), b = n2(foo = 2, bar = 3))"))
        }
        
        @Test
        fun `Given funky function, when parsing, then correctly parsed`()
        {
            val expression = listOf(
                ParameterNode("example", Variable("of")),
                ParameterNode(null, Function("derg", listOf(ParameterNode(null, Variable("syntax"))))),
            ).let { Function("is an", it) }
            
            assertEquals(Assign("this", expression).asList(), parse("this = `is an`(example = of, derg(syntax))"))
        }
        
        @Test
        fun `Given parenthesis, when parsing, then correctly parsed`()
        {
            assertEquals(1.e.asList(), parse("(1)"))
            assertEquals(Plus(1.e, 2.e).asList(), parse("1 + (2)"))
            assertEquals(Multiply(1.e, Plus(2.e, 3.e)).asList(), parse("1 * (2 + 3)"))
            assertEquals(Divide(Plus(1.e, 2.e), Variable("foo")).asList(), parse("(1 + 2) / foo"))
        }
        
        @Test
        fun `Given single operator, when parsing, then correctly parsed`()
        {
            // Assignment
            assertEquals(Assign("foo", 2.e).asList(), parse("foo = 2"))
            assertEquals(AssignPlus("foo", 2.e).asList(), parse("foo += 2"))
            assertEquals(AssignMinus("foo", 2.e).asList(), parse("foo -= 2"))
            assertEquals(AssignMultiply("foo", 2.e).asList(), parse("foo *= 2"))
            assertEquals(AssignDivide("foo", 2.e).asList(), parse("foo /= 2"))
            
            // Arithmetic
            assertEquals(Plus(1.e, 2.e).asList(), parse("1 + 2"))
            assertEquals(Minus(1.e, 2.e).asList(), parse("1 - 2"))
            assertEquals(Multiply(1.e, 2.e).asList(), parse("1 * 2"))
            assertEquals(Divide(1.e, 2.e).asList(), parse("1 / 2"))
            
            // Comparison
            assertEquals(Less(1.e, 2.e).asList(), parse("1 < 2"))
            assertEquals(LessEqual(1.e, 2.e).asList(), parse("1 <= 2"))
            assertEquals(Greater(1.e, 2.e).asList(), parse("1 > 2"))
            assertEquals(GreaterEqual(1.e, 2.e).asList(), parse("1 >= 2"))
            assertEquals(Equal(1.e, 2.e).asList(), parse("1 == 2"))
            assertEquals(NotEqual(1.e, 2.e).asList(), parse("1 != 2"))
            assertEquals(ThreeWay(1.e, 2.e).asList(), parse("1 <=> 2"))
            
            // Logical
            assertEquals(LogicalAnd(1.e, 2.e).asList(), parse("1 && 2"))
            assertEquals(LogicalOr(1.e, 2.e).asList(), parse("1 || 2"))
            assertEquals(LogicalXor(1.e, 2.e).asList(), parse("1 ^^ 2"))
    
            // Prefix operators
            assertEquals(Unary(1.e).asList(), parse("-1"))
            assertEquals(LogicalNot(true.e).asList(), parse("!true"))
            assertEquals(IncrementPre("foo").asList(), parse("++foo"))
            assertEquals(DecrementPre("foo").asList(), parse("--foo"))
            
            // Postfix operators
            assertEquals(IncrementPost("bar").asList(), parse("bar++"))
            assertEquals(DecrementPost("bar").asList(), parse("bar--"))
        }
        
        @Test
        fun `Given same operator precedence, when parsing, then correctly parsed`()
        {
            assertEquals(Plus(Minus(1.e, 2.e), 3.e).asList(), parse("1 - 2 + 3"))
            assertEquals(Minus(Plus(1.e, 2.e), 3.e).asList(), parse("1 + 2 - 3"))
            
            assertEquals(Multiply(Divide(1.e, 2.e), 3.e).asList(), parse("1 / 2 * 3"))
            assertEquals(Divide(Multiply(1.e, 2.e), 3.e).asList(), parse("1 * 2 / 3"))
        }
        
        @Test
        fun `Given different operator precedence, when parsing, then correctly parsed`()
        {
            // add/minus is lower than multiply/divide
            assertEquals(Plus(Multiply(1.e, 2.e), 3.e).asList(), parse("1 * 2 + 3"))
            assertEquals(Plus(1.e, Multiply(2.e, 3.e)).asList(), parse("1 + 2 * 3"))
        }
    }
}
