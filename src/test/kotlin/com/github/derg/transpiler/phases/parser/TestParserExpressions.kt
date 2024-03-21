package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import org.junit.jupiter.api.*

/**
 * Determines whether the current token stream is parsed correctly. The expectation is that there will be [preOkCount]
 * number of tokens resulting in [ParseOk.Complete], followed by [wipCount] [ParseOk.Incomplete], then followed by
 * [postOkCount] [ParseOk.Complete] again, before finally resulting in [ParseOk.Finished].
 */
private fun <Type> Tester<Type>.isChain(preOkCount: Int = 0, wipCount: Int = 0, postOkCount: Int = 0): Tester<Type> =
    isOk(preOkCount).isWip(wipCount).isOk(postOkCount).isDone()

class TestParserExpression
{
    private val tester = Tester { expressionParserOf() }
    
    @Test
    fun `Given valid token, when parsing, then correct expression`()
    {
        // Literal values
        tester.parse("true").isChain(1).isValue(true.ast).resets()
        tester.parse("false").isChain(1).isValue(false.ast).resets()
        tester.parse("42").isChain(1).isValue(42.ast).resets()
        tester.parse("42i32").isChain(1).isValue(42.ast).resets()
        tester.parse("42i64").isChain(1).isValue(42L.ast).resets()
        tester.parse("\"foo\"").isChain(1).isValue("foo".ast).resets()
        tester.parse("\"bar\"s").isChain(1).isValue("bar".ast).resets()
        
        // Accesses
        tester.parse("whatever").isChain(1).isValue("whatever".astRead).resets()
        tester.parse("f()").isChain(1, 1, 1).isValue("f".astCall()).resets()
        tester.parse("f(1)").isChain(1, 2, 1).isValue("f".astCall(1)).resets()
        tester.parse("f(1,)").isChain(1, 3, 1).isValue("f".astCall(1)).resets()
        tester.parse("f(1,2)").isChain(1, 4, 1).isValue("f".astCall(1, 2)).resets()
        tester.parse("f(bar = 1)").isChain(1, 4, 1).isValue("f".astCall("bar" to 1)).resets()
        
        // Structural
        tester.parse("(1)").isChain(0, 2, 1).isValue(1.ast).resets()
        tester.parse("(((1)))").isChain(0, 6, 1).isValue(1.ast).resets()
        
        // Operators
        tester.parse("1 + 2").isChain(1, 1, 1).isValue(1 astAdd 2).resets()
        tester.parse("1 - 2").isChain(1, 1, 1).isValue(1 astSub 2).resets()
        tester.parse("1 * 2").isChain(1, 1, 1).isValue(1 astMul 2).resets()
        tester.parse("1 / 2").isChain(1, 1, 1).isValue(1 astDiv 2).resets()
        tester.parse("1 % 2").isChain(1, 1, 1).isValue(1 astMod 2).resets()
        tester.parse("1 && 2").isChain(1, 1, 1).isValue(1 astAnd 2).resets()
        tester.parse("1 || 2").isChain(1, 1, 1).isValue(1 astOr 2).resets()
        tester.parse("1 ^^ 2").isChain(1, 1, 1).isValue(1 astXor 2).resets()
        tester.parse("1 == 2").isChain(1, 1, 1).isValue(1 astEq 2).resets()
        tester.parse("1 ~= 2").isChain(1, 1, 1).isValue(1 astNe 2).resets()
        tester.parse("1 < 2").isChain(1, 1, 1).isValue(1 astLt 2).resets()
        tester.parse("1 <= 2").isChain(1, 1, 1).isValue(1 astLe 2).resets()
        tester.parse("1 > 2").isChain(1, 1, 1).isValue(1 astGt 2).resets()
        tester.parse("1 >= 2").isChain(1, 1, 1).isValue(1 astGe 2).resets()
        tester.parse("1 <=> 2").isChain(1, 1, 1).isValue(1 astTw 2).resets()
        
        // Unary
        tester.parse("~1").isChain(0, 1, 1).isValue(1.astNot)
        tester.parse("+1").isChain(0, 1, 1).isValue(1.astPlus)
        tester.parse("-1").isChain(0, 1, 1).isValue(1.astMinus)
        
        // Error
        tester.parse("1 ! 2").isChain(1, 1, 1).isValue(1 astCatchRaise 2)
        tester.parse("1 ? 2").isChain(1, 1, 1).isValue(1 astCatchReturn 2)
        
        // When
        tester.parse("when 1 2 -> 3").isChain(0, 4, 1).isValue(astWhenOf(1, 2 to 3))
        tester.parse("when 1 2 -> 3 4 -> 5").isWip(4).isChain(1, 2, 1).isValue(astWhenOf(1, 2 to 3, 4 to 5))
        tester.parse("when 1 2 -> 3 else 4").isWip(4).isChain(1, 1, 1).isValue(astWhenOf(1, 2 to 3, default = 4))
    }
    
    @Test
    fun `Given valid token, when parsing, then correct precedence`()
    {
        // Operators
        tester.parse("1 + 2 - 3").step(5).isDone().isValue((1 astAdd 2) astSub 3)
        tester.parse("1 + 2 * 3").step(5).isDone().isValue(1 astAdd (2 astMul 3))
        tester.parse("1 - 2 < 6 / 3").step(7).isDone().isValue((1 astSub 2) astLt (6 astDiv 3))
        tester.parse("1 && 2 || 3 && 4").step(7).isDone().isValue((1 astAnd 2) astOr (3 astAnd 4))
        tester.parse("1 || 2 && 3 || 4").step(7).isDone().isValue(1 astOr ((2 astAnd 3) astOr 4))
        tester.parse("1 == 2 && 3").step(5).isDone().isValue((1 astEq 2) astAnd 3)
        tester.parse("1 == (2 && 3)").step(7).isDone().isValue(1 astEq (2 astAnd 3))
        
        // Unary
        tester.parse("~~1").step(3).isDone().isValue(1.astNot.astNot)
        tester.parse("+-1").step(3).isDone().isValue(1.astMinus.astPlus)
        
        // Mixed
        tester.parse("1 ++ 2").step(4).isDone().isValue(1 astAdd 2.astPlus)
        tester.parse("~1 * -2").step(5).isDone().isValue(1.astNot astMul 2.astMinus)
        tester.parse("-(1 * 2)").step(6).isDone().isValue((1 astMul 2).astMinus)
        tester.parse("1 ! 2 + 3").step(5).isDone().isValue(1 astCatchRaise (2 astAdd 3))
        tester.parse("1 ? 2 + 3").step(5).isDone().isValue(1 astCatchReturn (2 astAdd 3))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("*").isBad { ParseError.UnexpectedToken(it[0]) }
        tester.parse("when").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        
        // Structural errors
        tester.parse("(").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("()").isWip(1).isBad { ParseError.UnexpectedToken(it[1]) }
        tester.parse("(1").isWip(2).isBad { ParseError.UnexpectedToken(EndOfFile) }
        
        // Operator errors
        tester.parse("+").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("1 *").isOk(1).isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
