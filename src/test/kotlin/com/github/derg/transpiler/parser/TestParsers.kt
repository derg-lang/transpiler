package com.github.derg.transpiler.parser

import com.github.derg.transpiler.ast.Access
import com.github.derg.transpiler.ast.Operator.*
import com.github.derg.transpiler.ast.Value
import com.github.derg.transpiler.lexer.Structure.Type.ARROW
import com.github.derg.transpiler.util.failureOf
import com.github.derg.transpiler.util.isFailure
import com.github.derg.transpiler.util.successOf
import com.github.derg.transpiler.util.toSuccess
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

class TestExpressions
{
    /**
     * Boolean values are simple keywords which are treated specially. The special keywords resolve down to a value
     * type expression, which may be used in other expressions.
     */
    @Nested
    inner class Bool
    {
        @Test
        fun `Given valid values, when parsing, then correctly parsed`()
        {
            assertEquals(Value.Bool(true).toSuccess(), ParserBool.parse("true"))
            assertEquals(Value.Bool(false).toSuccess(), ParserBool.parse("false"))
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), ParserBool.parse(""))
            assertEquals(failureOf("'0' is not a keyword"), ParserBool.parse("0"))
            assertEquals(failureOf("'if' is not a bool"), ParserBool.parse("if"))
        }
    }
    
    /**
     * Real numbers are numeric quantities which may be given a unit to go along with the value. The only permitted
     * way to specify a custom literal type is to include the literal next to the value, with no space in-between
     * the literal and the value.
     */
    @Nested
    inner class Real
    {
        @Test
        fun `Given valid values, when parsing, then correctly parsed`()
        {
            assertEquals(Value.Real(1.toBigDecimal(), null).toSuccess(), ParserReal.parse("1"))
            assertEquals(Value.Real(3.14.toBigDecimal(), "s").toSuccess(), ParserReal.parse("3.14s"))
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), ParserReal.parse(""))
            assertEquals(failureOf("'test' is not a number"), ParserReal.parse("test"))
        }
    }
    
    /**
     * Text is a sequence of characters which may be given a unit to specify a certain type. The only permitted way
     * to specify a custom literal type is to include the literal next to the text, with no space in-between the
     * literal and the text.
     */
    @Nested
    inner class Text
    {
        @Test
        fun `Given valid values, when parsing, then correctly parsed`()
        {
            assertEquals(Value.Text("foo", null).toSuccess(), ParserText.parse(""""foo""""))
            assertEquals(Value.Text("bar", "s").toSuccess(), ParserText.parse(""""bar"s"""))
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), ParserText.parse(""))
            assertEquals(failureOf("'test' is not a string"), ParserText.parse("test"))
        }
    }
    
    /**
     * Any stand-alone identifier is interpreted to be the value stored in a variable. The meaning of the variable
     * is interpreted by the analyzer of the source code, rather than the parser.
     */
    @Nested
    inner class Variable
    {
        @Test
        fun `Given valid identifiers, when parsing, then correctly parsed`()
        {
            assertEquals(Access.Variable("test").toSuccess(), ParserVariable.parse("test"))
        }
        
        @Test
        fun `Given invalid identifiers, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), ParserVariable.parse(""))
            assertEquals(failureOf("'if' is not a variable"), ParserVariable.parse("if"))
        }
    }
    
    /**
     * The structure parser ensures that a specific expected structure token is found in the source which is being
     * parsed. Certain tokens are not required to resolve ambiguity, but aids in improving code readability, thus making
     * those tokens mandatory.
     */
    @Nested
    inner class Structure
    {
        private val pattern = ParseStructure(ARROW)
        
        @Test
        fun `Given valid values, when parsing, then correctly parsed`()
        {
            assertEquals(successOf(), pattern.parse("->"))
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected '->', found end of stream"), pattern.parse(""))
            assertEquals(failureOf("expected '->', found 'if'"), pattern.parse("if"))
        }
    }
    
    /**
     * Some operators act only on the expression to their right side. These expressions may be nested behind other
     * expressions involving prefix operators or other similar.
     */
    @Nested
    inner class PrefixOperator
    {
        private fun parse(source: String) = ParserPrefixOperator.parse(source)
        
        @Test
        fun `Given valid operator, when parsing, then correctly parsed`()
        {
            assertEquals(Not(1.value).toSuccess(), parse("!1"))
            assertEquals(UnaryPlus(1.value).toSuccess(), parse("+1"))
            assertEquals(UnaryMinus(1.value).toSuccess(), parse("-1"))
            assertEquals(PreIncrement("a".variable).toSuccess(), parse("++a"))
            assertEquals(PreDecrement("a".variable).toSuccess(), parse("--a"))
        }
        
        @Test
        fun `Given valid operator, when parsing, then correct precedence`()
        {
            assertEquals(Not(PreIncrement("a".variable)).toSuccess(), parse("!++a"))
        }
        
        @Test
        fun `Given invalid operator, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), parse(""))
            assertEquals(failureOf("'*' is not a legal prefix operator"), parse("*a"))
        }
    }
    
    /**
     * Some operators act only on the expression to their right side. These expressions may be nested behind other
     * expressions involving prefix operators or other similar.
     */
    @Nested
    inner class PostfixOperator
    {
        private fun parse(source: String) = ParserPostfixOperator.parse(source)
        
        @Test
        fun `Given valid operator, when parsing, then correctly parsed`()
        {
            assertEquals(PostIncrement("a".variable).toSuccess(), parse("a++"))
            assertEquals(PostDecrement("a".variable).toSuccess(), parse("a--"))
        }
        
        @Test
        fun `Given invalid operator, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), parse(""))
            assertEquals(failureOf("'*' is not a legal postfix operator"), parse("a*"))
            assertEquals(failureOf("'++' is not a legal postfix operator here"), parse("1++"))
        }
    }
    
    /**
     * Various forms of operators operating on two expressions require valid expressions. The expressions may be
     * affected by various precedence rules as well, requiring special attention to ensure expressions are constructed
     * properly when parsing.
     */
    @Nested
    inner class InfixOperator
    {
        private fun parse(source: String) = ParserInfixOperator.parse(source)
        
        @Test
        fun `Given valid operator, when parsing, then correctly parsed`()
        {
            assertEquals(Add(1.value, 2.value).toSuccess(), parse("1 + 2"))
            assertEquals(Subtract(1.value, 2.value).toSuccess(), parse("1 - 2"))
            assertEquals(Multiply(1.value, 2.value).toSuccess(), parse("1 * 2"))
            assertEquals(Divide(1.value, 2.value).toSuccess(), parse("1 / 2"))
            assertEquals(Modulo(1.value, 2.value).toSuccess(), parse("1 % 2"))
            
            assertEquals(Equal(1.value, 2.value).toSuccess(), parse("1 == 2"))
            assertEquals(NotEqual(1.value, 2.value).toSuccess(), parse("1 != 2"))
            assertEquals(Less(1.value, 2.value).toSuccess(), parse("1 < 2"))
            assertEquals(LessEqual(1.value, 2.value).toSuccess(), parse("1 <= 2"))
            assertEquals(Greater(1.value, 2.value).toSuccess(), parse("1 > 2"))
            assertEquals(GreaterEqual(1.value, 2.value).toSuccess(), parse("1 >= 2"))
            assertEquals(ThreeWay(1.value, 2.value).toSuccess(), parse("1 <=> 2"))
            
            assertEquals(And(1.value, 2.value).toSuccess(), parse("1 && 2"))
            assertEquals(Or(1.value, 2.value).toSuccess(), parse("1 || 2"))
            assertEquals(Xor(1.value, 2.value).toSuccess(), parse("1 ^^ 2"))
        }
        
        @Test
        fun `Given valid operator, when parsing, then correct precedence`()
        {
            // Operators with same precedence
            
            assertEquals(Assign("a".variable, AssignAdd("b".variable, 1.value)).toSuccess(), parse("a = b += 1"))
            assertEquals(AssignAdd("a".variable, Assign("b".variable, 1.value)).toSuccess(), parse("a += b = 1"))
            assertEquals(Assign("a".variable, AssignSubtract("b".variable, 1.value)).toSuccess(), parse("a = b -= 1"))
            assertEquals(AssignSubtract("a".variable, Assign("b".variable, 1.value)).toSuccess(), parse("a -= b = 1"))
            assertEquals(Assign("a".variable, AssignMultiply("b".variable, 1.value)).toSuccess(), parse("a = b *= 1"))
            assertEquals(AssignMultiply("a".variable, Assign("b".variable, 1.value)).toSuccess(), parse("a *= b = 1"))
            assertEquals(Assign("a".variable, AssignDivide("b".variable, 1.value)).toSuccess(), parse("a = b /= 1"))
            assertEquals(AssignDivide("a".variable, Assign("b".variable, 1.value)).toSuccess(), parse("a /= b = 1"))
            assertEquals(Assign("a".variable, AssignModulo("b".variable, 1.value)).toSuccess(), parse("a = b %= 1"))
            assertEquals(AssignModulo("a".variable, Assign("b".variable, 1.value)).toSuccess(), parse("a %= b = 1"))
            
            assertEquals(NotEqual(Equal(1.value, 2.value), 3.value).toSuccess(), parse("1 == 2 != 3"))
            assertEquals(Equal(NotEqual(1.value, 2.value), 3.value).toSuccess(), parse("1 != 2 == 3"))
            assertEquals(Less(Equal(1.value, 2.value), 3.value).toSuccess(), parse("1 == 2 < 3"))
            assertEquals(Equal(Less(1.value, 2.value), 3.value).toSuccess(), parse("1 < 2 == 3"))
            assertEquals(LessEqual(Equal(1.value, 2.value), 3.value).toSuccess(), parse("1 == 2 <= 3"))
            assertEquals(Equal(LessEqual(1.value, 2.value), 3.value).toSuccess(), parse("1 <= 2 == 3"))
            assertEquals(Greater(Equal(1.value, 2.value), 3.value).toSuccess(), parse("1 == 2 > 3"))
            assertEquals(Equal(Greater(1.value, 2.value), 3.value).toSuccess(), parse("1 > 2 == 3"))
            assertEquals(GreaterEqual(Equal(1.value, 2.value), 3.value).toSuccess(), parse("1 == 2 >= 3"))
            assertEquals(Equal(GreaterEqual(1.value, 2.value), 3.value).toSuccess(), parse("1 >= 2 == 3"))
            
            assertEquals(Subtract(Add(1.value, 2.value), 3.value).toSuccess(), parse("1 + 2 - 3"))
            assertEquals(Add(Subtract(1.value, 2.value), 3.value).toSuccess(), parse("1 - 2 + 3"))
            
            assertEquals(Divide(Multiply(1.value, 2.value), 3.value).toSuccess(), parse("1 * 2 / 3"))
            assertEquals(Multiply(Divide(1.value, 2.value), 3.value).toSuccess(), parse("1 / 2 * 3"))
            assertEquals(Modulo(Multiply(1.value, 2.value), 3.value).toSuccess(), parse("1 * 2 % 3"))
            assertEquals(Multiply(Modulo(1.value, 2.value), 3.value).toSuccess(), parse("1 % 2 * 3"))
            
            // Operators with different precedence
            
            assertEquals(And(1.value, Xor(2.value, 3.value)).toSuccess(), parse("1 && 2 ^^ 3"))
            assertEquals(And(Xor(1.value, 2.value), 3.value).toSuccess(), parse("1 ^^ 2 && 3"))
            
            assertEquals(Xor(1.value, Or(2.value, 3.value)).toSuccess(), parse("1 ^^ 2 || 3"))
            assertEquals(Xor(Or(1.value, 2.value), 3.value).toSuccess(), parse("1 || 2 ^^ 3"))
            
            assertEquals(Or(1.value, Equal(2.value, 3.value)).toSuccess(), parse("1 || 2 == 3"))
            assertEquals(Or(Equal(1.value, 2.value), 3.value).toSuccess(), parse("1 == 2 || 3"))
            
            assertEquals(Equal(1.value, ThreeWay(2.value, 3.value)).toSuccess(), parse("1 == 2 <=> 3"))
            assertEquals(Equal(ThreeWay(1.value, 2.value), 3.value).toSuccess(), parse("1 <=> 2 == 3"))
            
            assertEquals(ThreeWay(Add(1.value, 2.value), 3.value).toSuccess(), parse("1 + 2 <=> 3"))
            assertEquals(ThreeWay(1.value, Add(2.value, 3.value)).toSuccess(), parse("1 <=> 2 + 3"))
            
            assertEquals(Add(Multiply(1.value, 2.value), 3.value).toSuccess(), parse("1 * 2 + 3"))
            assertEquals(Add(1.value, Multiply(2.value, 3.value)).toSuccess(), parse("1 + 2 * 3"))
            
            // Super-special cases where precedence *is* different, but we have right-to-left associativity
            
            assertEquals(Assign("a".variable, And(1.value, 2.value)).toSuccess(), parse("a = 1 && 2"))
            assertEquals(And(1.value, Assign("a".variable, 2.value)).toSuccess(), parse("1 && a = 2"))
        }
        
        @Test
        fun `Given invalid operator, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), parse(""))
            assertEquals(failureOf("expected token, found end of stream"), parse("1"))
            assertEquals(failureOf("expected token, found end of stream"), parse("1 +"))
            assertEquals(failureOf("'if' is not an operator"), parse("1 if 2"))
            assertEquals(failureOf("'!' is not a legal operator"), parse("1 ! 2"))
            assertTrue(parse("1 + if").isFailure)
        }
    }
    
    /**
     * The usage of parenthesis avoids ambiguity and forced a different evaluation order when parsing expressions, and
     * as such are a critical component to take into consideration when parsing source code.
     */
    @Nested
    inner class Parenthesis
    {
        private fun parse(source: String) = ParserParenthesis.parse(source)
        
        @Test
        fun `Given valid parenthesis, when parsing, then correctly parsed`()
        {
            assertEquals(1.value.toSuccess(), parse("(1)"))
            assertEquals(Add(1.value, 2.value).toSuccess(), parse("(1 + 2)"))
            assertEquals(Multiply(1.value, Add(2.value, 3.value)).toSuccess(), parse("(1 * (2 + 3))"))
        }
        
        @Test
        fun `Given invalid operator, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected '(', found end of stream"), parse(""))
            assertEquals(failureOf("expected token, found end of stream"), parse("("))
            assertEquals(failureOf("expected ')', found end of stream"), parse("(1"))
        }
    }
}
