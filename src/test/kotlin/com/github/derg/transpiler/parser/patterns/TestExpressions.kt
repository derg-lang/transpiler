package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Assignment
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
        private val tester = ParserTester { ParserExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("true").isGood(1, true.toLit())
            tester.parse("1").isGood(1, 1.toLit())
            tester.parse("\"\"").isGood(1, "".toLit())
            tester.parse("foo").isGood(1, "foo".toVar())
            tester.parse("(2)").isGood(3, 2.toLit())
            tester.parse("f()").isGood(3, "f".toFun())
            tester.parse("-1").isGood(2, opUnMinus(1))
            tester.parse("1 + 2").isGood(3, 1 opAdd 2)
            tester.parse("a = 1").isGood(3, Assignment.Assign("a", 1.toLit()))
            tester.parse("1 + 2").isGood(3, 1 opAdd 2)
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
        private val tester = ParserTester { ParserBoolExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("true").isGood(1, true.toLit())
            tester.parse("false").isGood(1, false.toLit())
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
        private val tester = ParserTester { ParserRealExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("42").isGood(1, 42.toLit())
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
        private val tester = ParserTester { ParserTextExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("\"whatever\"").isGood(1, "whatever".toLit())
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
        private val tester = ParserTester { ParserVariableExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("foo").isGood(1, "foo".toVar())
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
        private val tester = ParserTester { ParserFunctionExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("f()").isGood(3, "f".toFun())
            tester.parse("f(1)").isGood(4, "f".toFun(1.toPar()))
            tester.parse("f(1, 2)").isGood(6, "f".toFun(1.toPar(), 2.toPar()))
            tester.parse("f(1, 2, )").isGood(7, "f".toFun(1.toPar(), 2.toPar()))
            tester.parse("f(foo = 1)").isGood(6, "f".toFun(1.toPar("foo")))
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
        private val tester = ParserTester { ParserParenthesisExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("(1)").isGood(3, 1.toLit())
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
        private val tester = ParserTester { ParserPrefixOperatorExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("!1").isGood(2, opNot(1))
            tester.parse("+1").isGood(2, opUnPlus(1))
            tester.parse("-1").isGood(2, opUnMinus(1))
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
        private val tester = ParserTester { ParserInfixOperatorExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            // Comparison
            tester.parse("1 == 2").isGood(3, 1 opEq 2)
            tester.parse("1 != 2").isGood(3, 1 opNe 2)
            tester.parse("1 < 2").isGood(3, 1 opLt 2)
            tester.parse("1 <= 2").isGood(3, 1 opLe 2)
            tester.parse("1 > 2").isGood(3, 1 opGt 2)
            tester.parse("1 >= 2").isGood(3, 1 opGe 2)
            tester.parse("1 <=> 2").isGood(3, 1 opTw 2)
            
            // Logical
            tester.parse("1 && 2").isGood(3, 1 opAnd 2)
            tester.parse("1 || 2").isGood(3, 1 opOr 2)
            tester.parse("1 ^^ 2").isGood(3, 1 opXor 2)
            
            // Arithmetic
            tester.parse("1 + 2").isGood(3, 1 opAdd 2)
            tester.parse("1 - 2").isGood(3, 1 opSub 2)
            tester.parse("1 * 2").isGood(3, 1 opMul 2)
            tester.parse("1 / 2").isGood(3, 1 opDiv 2)
            tester.parse("1 % 2").isGood(3, 1 opMod 2)
        }
        
        @Test
        fun `Given token precedence, when parsing, then correct precedence`()
        {
            tester.parse("1 + 2 * 3").isGood(5, 1 opAdd (2 opMul 3))
            tester.parse("1 * 2 + 3").isGood(5, (1 opMul 2) opAdd 3)
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
        private val tester = ParserTester { ParserAssignExpression }
        
        @Test
        fun `Given valid token, when parsing, then correctly parsed`()
        {
            tester.parse("a = 1").isGood(3, Assignment.Assign("a", 1.toLit()))
            tester.parse("a += 1").isGood(3, Assignment.AssignAdd("a", 1.toLit()))
            tester.parse("a -= 1").isGood(3, Assignment.AssignSubtract("a", 1.toLit()))
            tester.parse("a *= 1").isGood(3, Assignment.AssignMultiply("a", 1.toLit()))
            tester.parse("a /= 1").isGood(3, Assignment.AssignDivide("a", 1.toLit()))
            tester.parse("a %= 1").isGood(3, Assignment.AssignModulo("a", 1.toLit()))
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
}
