package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.lexer.EndOfFile
import com.github.derg.transpiler.parser.*
import org.junit.jupiter.api.Test

/**
 * Determines whether the current token stream is parsed correctly. The expectation is that there will be [preOkCount]
 * number of tokens resulting in [ParseOk.Complete], followed by [wipCount] [ParseOk.Incomplete], then followed by
 * [postOkCount] [ParseOk.Complete] again, before finally resulting in [ParseOk.Finished].
 */
private fun <Type> Tester<Type>.isChain(preOkCount: Int = 0, wipCount: Int = 0, postOkCount: Int = 0): Tester<Type> =
    isOk(preOkCount).isWip(wipCount).isOk(postOkCount).isDone()

class TestParserExpression
{
    private val tester = Tester { ParserExpression() }
    
    @Test
    fun `Given valid token, when parsing, then correct expression`()
    {
        // Literal values
        tester.parse("true").isChain(1).isValue(true.toExp()).resets()
        tester.parse("false").isChain(1).isValue(false.toExp()).resets()
        tester.parse("42").isChain(1).isValue(42.toExp()).resets()
        tester.parse("42f").isChain(1).isValue(42.toExp("f")).resets()
        tester.parse("\"foo\"").isChain(1).isValue("foo".toExp()).resets()
        tester.parse("\"bar\"f").isChain(1).isValue("bar".toExp("f")).resets()
        
        // Accesses
        tester.parse("whatever").isChain(1).isValue("whatever".toVar()).resets()
        tester.parse("f()").isChain(1, 1, 1).isValue("f".toFun()).resets()
        tester.parse("f(1)").isChain(1, 2, 1).isValue("f".toFun(1.toPar())).resets()
        tester.parse("f(1,)").isChain(1, 3, 1).isValue("f".toFun(1.toPar())).resets()
        tester.parse("f(1,2)").isChain(1, 4, 1).isValue("f".toFun(1.toPar(), 2.toPar())).resets()
        tester.parse("f(bar = 1)").isChain(1, 4, 1).isValue("f".toFun(1.toPar("bar"))).resets()
        tester.parse("f[]").isChain(1, 1, 1).isValue("f".toSub()).resets()
        tester.parse("f[1]").isChain(1, 2, 1).isValue("f".toSub(1.toPar())).resets()
        tester.parse("f[1,]").isChain(1, 3, 1).isValue("f".toSub(1.toPar())).resets()
        tester.parse("f[1,2]").isChain(1, 4, 1).isValue("f".toSub(1.toPar(), 2.toPar())).resets()
        tester.parse("f[bar = 1]").isChain(1, 4, 1).isValue("f".toSub(1.toPar("bar"))).resets()
        
        // Structural
        tester.parse("(1)").isChain(0, 2, 1).isValue(1.toExp()).resets()
        tester.parse("(((1)))").isChain(0, 6, 1).isValue(1.toExp()).resets()
        
        // Operators
        tester.parse("1 + 2").isChain(1, 1, 1).isValue(1 opAdd 2).resets()
        tester.parse("1 - 2").isChain(1, 1, 1).isValue(1 opSub 2).resets()
        tester.parse("1 * 2").isChain(1, 1, 1).isValue(1 opMul 2).resets()
        tester.parse("1 / 2").isChain(1, 1, 1).isValue(1 opDiv 2).resets()
        tester.parse("1 % 2").isChain(1, 1, 1).isValue(1 opMod 2).resets()
        tester.parse("1 && 2").isChain(1, 1, 1).isValue(1 opAnd 2).resets()
        tester.parse("1 || 2").isChain(1, 1, 1).isValue(1 opOr 2).resets()
        tester.parse("1 ^^ 2").isChain(1, 1, 1).isValue(1 opXor 2).resets()
        tester.parse("1 == 2").isChain(1, 1, 1).isValue(1 opEq 2).resets()
        tester.parse("1 != 2").isChain(1, 1, 1).isValue(1 opNe 2).resets()
        tester.parse("1 < 2").isChain(1, 1, 1).isValue(1 opLt 2).resets()
        tester.parse("1 <= 2").isChain(1, 1, 1).isValue(1 opLe 2).resets()
        tester.parse("1 > 2").isChain(1, 1, 1).isValue(1 opGt 2).resets()
        tester.parse("1 >= 2").isChain(1, 1, 1).isValue(1 opGe 2).resets()
        tester.parse("1 <=> 2").isChain(1, 1, 1).isValue(1 opTw 2).resets()
        
        // Unary
        tester.parse("~1").isChain(0, 1, 1).isValue(opNot(1))
        tester.parse("+1").isChain(0, 1, 1).isValue(opPlus(1))
        tester.parse("-1").isChain(0, 1, 1).isValue(opMinus(1))
    }
    
    @Test
    fun `Given valid token, when parsing, then correct precedence`()
    {
        // Operators
        tester.parse("1 + 2 - 3").step(5).isDone().isValue((1 opAdd 2) opSub 3)
        tester.parse("1 + 2 * 3").step(5).isDone().isValue(1 opAdd (2 opMul 3))
        tester.parse("1 - 2 < 6 / 3").step(7).isDone().isValue((1 opSub 2) opLt (6 opDiv 3))
        tester.parse("1 && 2 || 3 && 4").step(7).isDone().isValue((1 opAnd 2) opOr (3 opAnd 4))
        tester.parse("1 == 2 && 3").step(5).isDone().isValue((1 opEq 2) opAnd 3)
        tester.parse("1 == (2 && 3)").step(7).isDone().isValue(1 opEq (2 opAnd 3))
        
        // Unary
        tester.parse("~~1").step(3).isDone().isValue(opNot(opNot(1)))
        tester.parse("+-1").step(3).isDone().isValue(opPlus(opMinus(1)))
        
        // Mixed
        tester.parse("1 ++ 2").step(4).isDone().isValue(1 opAdd opPlus(2))
        tester.parse("~1 * -2").step(5).isDone().isValue(opNot(1) opMul opMinus(2))
        tester.parse("-(1 * 2)").step(6).isDone().isValue(opMinus(1 opMul 2))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("*").isBad { ParseError.UnexpectedToken(it[0]) }
        
        // Structural errors
        tester.parse("(").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("()").isWip(1).isBad { ParseError.UnexpectedToken(it[1]) }
        tester.parse("(1").isWip(2).isBad { ParseError.UnexpectedToken(EndOfFile) }
        
        // Operator errors
        tester.parse("+").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("1 *").isOk(1).isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
