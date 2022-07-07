package com.github.derg.transpiler.lexer

import com.github.derg.transpiler.core.Node
import com.github.derg.transpiler.core.NodeExpression
import com.github.derg.transpiler.core.NodeExpression.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

private fun Node.asList() = listOf(this)

private val Boolean.e get() = Bool(this)
private val Int.e get() = NodeExpression.Numeric(toBigDecimal())
private val Double.e get() = NodeExpression.Numeric(toBigDecimal())
private val String.e get() = NodeExpression.Textual(this)

/**
 * Helper for parsing a [input] source code string into an abstract syntax tree.
 */
private fun parse(input: String): List<Node> = parse(tokenize(input))

class TestLexer
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
        }
        
        @Test
        fun `Given unary operator, when parsing, then correctly parsed`()
        {
            assertEquals(Unary(1.e).asList(), parse("-1"))
            assertEquals(Unary("foo".e).asList(), parse(""" -"foo" """))
        }
        
        @Test
        fun `Given negate operator, when parsing, then correctly parsed`()
        {
            assertEquals(Negate(true.e).asList(), parse("!true"))
            assertEquals(Negate(2.e).asList(), parse("!2"))
        }
        
        @Test
        fun `Given increment or decrement operator, when parsing, then correctly parsed`()
        {
            assertEquals(IncrementPre("foo").asList(), parse("++foo"))
            assertEquals(IncrementPost("bar").asList(), parse("bar++"))
            assertEquals(DecrementPre("foo").asList(), parse("--foo"))
            assertEquals(DecrementPost("bar").asList(), parse("bar--"))
        }
        
        @Test
        fun `Given single operator, when parsing, then correctly parsed`()
        {
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
            assertEquals(And(1.e, 2.e).asList(), parse("1 && 2"))
            assertEquals(Or(1.e, 2.e).asList(), parse("1 || 2"))
            assertEquals(Xor(1.e, 2.e).asList(), parse("1 ^^ 2"))
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
