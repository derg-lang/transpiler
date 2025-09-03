package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

class TestInterpreter
{
    private val interpreter = Interpreter(Builtin.environment)
    
    @Test
    fun `Given primitive expressions, when evaluating, they evaluate to themselves`()
    {
        assertSuccess(0.thir, interpreter.evaluate(0.thir))
        assertSuccess(1.thir, interpreter.evaluate(1.thir))
        assertSuccess(3.14.thir, interpreter.evaluate(3.14.thir))
        assertSuccess(true.thir, interpreter.evaluate(true.thir))
        assertSuccess("".thir, interpreter.evaluate("".thir))
    }
    
    @Test
    fun `Given builtin functions, when evaluating, they evaluate correctly`()
    {
        assertSuccess(false.thir, interpreter.evaluate(true thirAnd false))
        assertSuccess(false.thir, interpreter.evaluate(true thirEq false))
        assertSuccess(true.thir, interpreter.evaluate(true thirNe false))
        assertSuccess(false.thir, interpreter.evaluate(true.thirNot))
        assertSuccess(true.thir, interpreter.evaluate(true thirOr false))
        assertSuccess(true.thir, interpreter.evaluate(true thirXor false))
        
        assertSuccess(false.thir, interpreter.evaluate(1 thirEq 2))
        assertSuccess(false.thir, interpreter.evaluate(1 thirGe 2))
        assertSuccess(false.thir, interpreter.evaluate(1 thirGt 2))
        assertSuccess(true.thir, interpreter.evaluate(1 thirLe 2))
        assertSuccess(true.thir, interpreter.evaluate(1 thirLt 2))
        assertSuccess(true.thir, interpreter.evaluate(1 thirNe 2))
        assertSuccess(3.thir, interpreter.evaluate(1 thirAdd 2))
        assertSuccess(0.thir, interpreter.evaluate(1 thirDiv 2))
        assertSuccess(1.thir, interpreter.evaluate(1 thirMod 2))
        assertSuccess(2.thir, interpreter.evaluate(1 thirMul 2))
        assertSuccess((-1).thir, interpreter.evaluate(1.thirMinus))
        assertSuccess(1.thir, interpreter.evaluate(1.thirPlus))
        assertSuccess((-1).thir, interpreter.evaluate(1 thirSub 2))
        
        assertSuccess(false.thir, interpreter.evaluate(1L thirEq 2L))
        assertSuccess(false.thir, interpreter.evaluate(1L thirGe 2L))
        assertSuccess(false.thir, interpreter.evaluate(1L thirGt 2L))
        assertSuccess(true.thir, interpreter.evaluate(1L thirLe 2L))
        assertSuccess(true.thir, interpreter.evaluate(1L thirLt 2L))
        assertSuccess(true.thir, interpreter.evaluate(1L thirNe 2L))
        assertSuccess(3L.thir, interpreter.evaluate(1L thirAdd 2L))
        assertSuccess(0L.thir, interpreter.evaluate(1L thirDiv 2L))
        assertSuccess(1L.thir, interpreter.evaluate(1L thirMod 2L))
        assertSuccess(2L.thir, interpreter.evaluate(1L thirMul 2L))
        assertSuccess((-1L).thir, interpreter.evaluate(1L.thirMinus))
        assertSuccess(1L.thir, interpreter.evaluate(1L.thirPlus))
        assertSuccess((-1L).thir, interpreter.evaluate(1L thirSub 2L))
        
        assertSuccess(false.thir, interpreter.evaluate(1.0f thirEq 2.0f))
        assertSuccess(false.thir, interpreter.evaluate(1.0f thirGe 2.0f))
        assertSuccess(false.thir, interpreter.evaluate(1.0f thirGt 2.0f))
        assertSuccess(true.thir, interpreter.evaluate(1.0f thirLe 2.0f))
        assertSuccess(true.thir, interpreter.evaluate(1.0f thirLt 2.0f))
        assertSuccess(true.thir, interpreter.evaluate(1.0f thirNe 2.0f))
        assertSuccess(3.0f.thir, interpreter.evaluate(1.0f thirAdd 2.0f))
        assertSuccess(2.0f.thir, interpreter.evaluate(1.0f thirMul 2.0f))
        assertSuccess(0.5f.thir, interpreter.evaluate(1.0f thirDiv 2.0f))
        assertSuccess(1.0f.thir, interpreter.evaluate(1.0f thirMod 2.0f))
        assertSuccess((-1.0f).thir, interpreter.evaluate(1.0f.thirMinus))
        assertSuccess(1.0f.thir, interpreter.evaluate(1.0f.thirPlus))
        assertSuccess((-1.0f).thir, interpreter.evaluate(1.0f thirSub 2.0f))
        
        assertSuccess(false.thir, interpreter.evaluate(1.0 thirEq 2.0))
        assertSuccess(false.thir, interpreter.evaluate(1.0 thirGe 2.0))
        assertSuccess(false.thir, interpreter.evaluate(1.0 thirGt 2.0))
        assertSuccess(true.thir, interpreter.evaluate(1.0 thirLe 2.0))
        assertSuccess(true.thir, interpreter.evaluate(1.0 thirLt 2.0))
        assertSuccess(true.thir, interpreter.evaluate(1.0 thirNe 2.0))
        assertSuccess(3.0.thir, interpreter.evaluate(1.0 thirAdd 2.0))
        assertSuccess(0.5.thir, interpreter.evaluate(1.0 thirDiv 2.0))
        assertSuccess(1.0.thir, interpreter.evaluate(1.0 thirMod 2.0))
        assertSuccess(2.0.thir, interpreter.evaluate(1.0 thirMul 2.0))
        assertSuccess((-1.0).thir, interpreter.evaluate(1.0.thirMinus))
        assertSuccess(1.0.thir, interpreter.evaluate(1.0.thirPlus))
        assertSuccess((-1.0).thir, interpreter.evaluate(1.0 thirSub 2.0))
        
        assertSuccess(false.thir, interpreter.evaluate("foo" thirEq "bar"))
        assertSuccess(true.thir, interpreter.evaluate("foo" thirNe "bar"))
        assertSuccess("foobar".thir, interpreter.evaluate("foo" thirAdd "bar"))
    }
    
    @Test
    fun `Given custom function, when returning, then correct value is returned`()
    {
        val nothing = thirFunOf(valueType = ThirType.Void, errorType = ThirType.Void, statements = listOf(ThirStatement.Return))
        val value = thirFunOf(valueType = ThirType.Int32, errorType = ThirType.Void, statements = listOf(1.thirReturnValue))
        val error = thirFunOf(valueType = ThirType.Void, errorType = ThirType.Int32, statements = listOf(2.thirReturnError))
        
        interpreter.registerFunction(nothing)
        interpreter.registerFunction(value)
        interpreter.registerFunction(error)
        
        assertSuccess(null, interpreter.evaluate(nothing.thirCall()))
        assertSuccess(1.thir, interpreter.evaluate(value.thirCall()))
        assertFailure(2.thir, interpreter.evaluate(error.thirCall()))
    }
}
