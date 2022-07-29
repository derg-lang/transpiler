package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Access
import com.github.derg.transpiler.ast.Assignment
import com.github.derg.transpiler.ast.Operator
import com.github.derg.transpiler.ast.Parameter
import com.github.derg.transpiler.lexer.Structure.Type.CLOSE_PARENTHESIS
import com.github.derg.transpiler.lexer.Structure.Type.OPEN_PARENTHESIS
import com.github.derg.transpiler.parser.*
import com.github.derg.transpiler.parser.ParseError.*
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

class TestParseExpressions
{
    @Nested
    inner class TestParserExpression
    {
        private val tester = PatternTester { ParserExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("true").isGood(1, true.value)
            tester.parse("1").isGood(1, 1.value)
            tester.parse("\"\"").isGood(1, "".value)
            tester.parse("foo").isGood(1, "foo".variable)
            tester.parse("(2)").isGood(3, 2.value)
            tester.parse("f()").isGood(3, "f".function)
            tester.parse("-1").isGood(2, Operator.UnaryMinus(1.value))
            tester.parse("1 + 2").isGood(3, Operator.Add(1.value, 2.value))
            tester.parse("a = 1").isGood(3, Assignment.Assign("a", 1.value))
            tester.parse("++a").isGood(2, Assignment.PreIncrement("a"))
            tester.parse("a + b").isGood(3, Operator.Add("a".variable, "b".variable))
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("if").isBad { NotExpression(it[0]) }
        }
    }
    
    @Nested
    inner class TestParserBoolExpression
    {
        private val tester = PatternTester { ParserBoolExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("true").isGood(1, true.value)
            tester.parse("false").isGood(1, false.value)
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("42").isBad { NotExpression(it[0]) }
            tester.parse("if").isBad { NotExpression(it[0]) }
        }
    }
    
    @Nested
    inner class TestParserRealExpression
    {
        private val tester = PatternTester { ParserRealExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("42").isGood(1, 42.value)
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("if").isBad { NotExpression(it[0]) }
        }
    }
    
    @Nested
    inner class TestParserTextExpression
    {
        private val tester = PatternTester { ParserTextExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("\"whatever\"").isGood(1, "whatever".value)
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("if").isBad { NotExpression(it[0]) }
        }
    }
    
    @Nested
    inner class TestParserVariableExpression
    {
        private val tester = PatternTester { ParserVariableExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("foo").isGood(1, "foo".variable)
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("false").isBad { NotIdentifier(it[0]) }
        }
    }
    
    @Nested
    inner class TestParserFunctionExpression
    {
        private val tester = PatternTester { ParserFunctionExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("f()").isGood(3, "f".function)
            tester.parse("f(1)").isGood(4, Access.Function("f", listOf(1.parameter)))
            tester.parse("f(1, 2)").isGood(6, Access.Function("f", listOf(1.parameter, 2.parameter)))
            tester.parse("f(1, 2, )").isGood(7, Access.Function("f", listOf(1.parameter, 2.parameter)))
            tester.parse("f(foo = 1)").isGood(6, Access.Function("f", listOf(Parameter("foo", 1.value))))
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("false").isBad { NotIdentifier(it[0]) }
        }
    }
    
    @Nested
    inner class TestParserParenthesisExpression
    {
        private val tester = PatternTester { ParserParenthesisExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("(1)").isGood(3, 1.value)
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("(").isBad { End }
            tester.parse("(1").isBad { End }
            tester.parse("if").isBad { NotStructure(it[0]) }
            tester.parse("(1(").isBad { WrongStructure(setOf(CLOSE_PARENTHESIS), OPEN_PARENTHESIS) }
        }
    }
    
    @Nested
    inner class TestParserPrefixOperatorExpression
    {
        private val tester = PatternTester { ParserPrefixOperatorExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("!1").isGood(2, Operator.Not(1.value))
            tester.parse("+1").isGood(2, Operator.UnaryPlus(1.value))
            tester.parse("-1").isGood(2, Operator.UnaryMinus(1.value))
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("!").isBad { End }
            tester.parse("if").isBad { NotOperator(it[0]) }
        }
    }
    
    @Nested
    inner class TestParserInfixOperatorExpression
    {
        private val tester = PatternTester { ParserInfixOperatorExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            // Comparison
            tester.parse("1 == 2").isGood(3, Operator.Equal(1.value, 2.value))
            tester.parse("1 != 2").isGood(3, Operator.NotEqual(1.value, 2.value))
            tester.parse("1 < 2").isGood(3, Operator.Less(1.value, 2.value))
            tester.parse("1 <= 2").isGood(3, Operator.LessEqual(1.value, 2.value))
            tester.parse("1 > 2").isGood(3, Operator.Greater(1.value, 2.value))
            tester.parse("1 >= 2").isGood(3, Operator.GreaterEqual(1.value, 2.value))
            tester.parse("1 <=> 2").isGood(3, Operator.ThreeWay(1.value, 2.value))
            
            // Logical
            tester.parse("1 && 2").isGood(3, Operator.And(1.value, 2.value))
            tester.parse("1 || 2").isGood(3, Operator.Or(1.value, 2.value))
            tester.parse("1 ^^ 2").isGood(3, Operator.Xor(1.value, 2.value))
            
            // Arithmetic
            tester.parse("1 + 2").isGood(3, Operator.Add(1.value, 2.value))
            tester.parse("1 - 2").isGood(3, Operator.Subtract(1.value, 2.value))
            tester.parse("1 * 2").isGood(3, Operator.Multiply(1.value, 2.value))
            tester.parse("1 / 2").isGood(3, Operator.Divide(1.value, 2.value))
            tester.parse("1 % 2").isGood(3, Operator.Modulo(1.value, 2.value))
        }
        
        @Test
        fun `Given token precedence, when parsing, then correct precedence`()
        {
            tester.parse("1 + 2 * 3").isGood(5, Operator.Add(1.value, Operator.Multiply(2.value, 3.value)))
            tester.parse("1 * 2 + 3").isGood(5, Operator.Add(Operator.Multiply(1.value, 2.value), 3.value))
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("2").isBad { End }
            tester.parse("3 +").isBad { End }
            tester.parse("if").isBad { NotExpression(it[0]) }
        }
    }
    
    @Nested
    inner class TestParserAssignExpression
    {
        private val tester = PatternTester { ParserAssignExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("a = 1").isGood(3, Assignment.Assign("a", 1.value))
            tester.parse("a += 1").isGood(3, Assignment.AssignAdd("a", 1.value))
            tester.parse("a -= 1").isGood(3, Assignment.AssignSubtract("a", 1.value))
            tester.parse("a *= 1").isGood(3, Assignment.AssignMultiply("a", 1.value))
            tester.parse("a /= 1").isGood(3, Assignment.AssignDivide("a", 1.value))
            tester.parse("a %= 1").isGood(3, Assignment.AssignModulo("a", 1.value))
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("foo").isBad { End }
            tester.parse("foo = ").isBad { End }
            tester.parse("if").isBad { NotIdentifier(it[0]) }
        }
    }
    
    @Nested
    inner class TestParserIncrementExpression
    {
        private val tester = PatternTester { ParserIncrementExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("++a").isGood(2, Assignment.PreIncrement("a"))
            tester.parse("--a").isGood(2, Assignment.PreDecrement("a"))
            tester.parse("a++").isGood(2, Assignment.PostIncrement("a"))
            tester.parse("a--").isGood(2, Assignment.PostDecrement("a"))
        }
        
        @Test
        fun `Given invalid token, when parsing, then correct error`()
        {
            tester.parse("").isBad { End }
            tester.parse("++").isBad { End }
            tester.parse("if").isBad { NotOperator(it[0]) }
        }
    }
}
