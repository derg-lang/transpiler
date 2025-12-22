package com.github.derg.transpiler.phases.interpreter

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

class TestEvaluator
{
    private val env = Builtin.generateEnvironment()
    private val globals = Builtin.generateGlobals()
    private val evaluator = Evaluator(env, globals)
    
    @Nested
    inner class Expressions
    {
        @Test
        fun `Given canonical expressions, when evaluating, then correct`()
        {
            assertSuccess(true.thir, evaluator.evaluate(true.thir))
            assertSuccess(2.thir, evaluator.evaluate(2.thir))
            assertSuccess(42L.thir, evaluator.evaluate(42L.thir))
            assertSuccess(3.14f.thir, evaluator.evaluate(3.14f.thir))
            assertSuccess(0.5.thir, evaluator.evaluate(0.5.thir))
            assertSuccess("foo".thir, evaluator.evaluate("foo".thir))
        }
        
        @Test
        fun `Given builtin structures, when evaluating, they evaluate correctly`()
        {
            assertSuccess(ThirType.Bool.thir, evaluator.evaluate(Builtin.BOOL.thirLoad()))
            assertSuccess(ThirType.Int32.thir, evaluator.evaluate(Builtin.INT32.thirLoad()))
            assertSuccess(ThirType.Int64.thir, evaluator.evaluate(Builtin.INT64.thirLoad()))
            assertSuccess(ThirType.Float32.thir, evaluator.evaluate(Builtin.FLOAT32.thirLoad()))
            assertSuccess(ThirType.Float64.thir, evaluator.evaluate(Builtin.FLOAT64.thirLoad()))
            assertSuccess(ThirType.Str.thir, evaluator.evaluate(Builtin.STR.thirLoad()))
        }
        
        @Test
        fun `Given builtin functions, when evaluating, they evaluate correctly`()
        {
            assertSuccess(false.thir, evaluator.evaluate(true thirAnd false))
            assertSuccess(false.thir, evaluator.evaluate(true thirEq false))
            assertSuccess(true.thir, evaluator.evaluate(true thirNe false))
            assertSuccess(false.thir, evaluator.evaluate(true.thirNot))
            assertSuccess(true.thir, evaluator.evaluate(true thirOr false))
            assertSuccess(true.thir, evaluator.evaluate(true thirXor false))
            
            assertSuccess(false.thir, evaluator.evaluate(1 thirEq 2))
            assertSuccess(false.thir, evaluator.evaluate(1 thirGe 2))
            assertSuccess(false.thir, evaluator.evaluate(1 thirGt 2))
            assertSuccess(true.thir, evaluator.evaluate(1 thirLe 2))
            assertSuccess(true.thir, evaluator.evaluate(1 thirLt 2))
            assertSuccess(true.thir, evaluator.evaluate(1 thirNe 2))
            assertSuccess(3.thir, evaluator.evaluate(1 thirAdd 2))
            assertSuccess(0.thir, evaluator.evaluate(1 thirDiv 2))
            assertSuccess(1.thir, evaluator.evaluate(1 thirMod 2))
            assertSuccess(2.thir, evaluator.evaluate(1 thirMul 2))
            assertSuccess((-1).thir, evaluator.evaluate(1.thirMinus))
            assertSuccess(1.thir, evaluator.evaluate(1.thirPlus))
            assertSuccess((-1).thir, evaluator.evaluate(1 thirSub 2))
            
            assertSuccess(false.thir, evaluator.evaluate(1L thirEq 2L))
            assertSuccess(false.thir, evaluator.evaluate(1L thirGe 2L))
            assertSuccess(false.thir, evaluator.evaluate(1L thirGt 2L))
            assertSuccess(true.thir, evaluator.evaluate(1L thirLe 2L))
            assertSuccess(true.thir, evaluator.evaluate(1L thirLt 2L))
            assertSuccess(true.thir, evaluator.evaluate(1L thirNe 2L))
            assertSuccess(3L.thir, evaluator.evaluate(1L thirAdd 2L))
            assertSuccess(0L.thir, evaluator.evaluate(1L thirDiv 2L))
            assertSuccess(1L.thir, evaluator.evaluate(1L thirMod 2L))
            assertSuccess(2L.thir, evaluator.evaluate(1L thirMul 2L))
            assertSuccess((-1L).thir, evaluator.evaluate(1L.thirMinus))
            assertSuccess(1L.thir, evaluator.evaluate(1L.thirPlus))
            assertSuccess((-1L).thir, evaluator.evaluate(1L thirSub 2L))
            
            assertSuccess(false.thir, evaluator.evaluate(1.0f thirEq 2.0f))
            assertSuccess(false.thir, evaluator.evaluate(1.0f thirGe 2.0f))
            assertSuccess(false.thir, evaluator.evaluate(1.0f thirGt 2.0f))
            assertSuccess(true.thir, evaluator.evaluate(1.0f thirLe 2.0f))
            assertSuccess(true.thir, evaluator.evaluate(1.0f thirLt 2.0f))
            assertSuccess(true.thir, evaluator.evaluate(1.0f thirNe 2.0f))
            assertSuccess(3.0f.thir, evaluator.evaluate(1.0f thirAdd 2.0f))
            assertSuccess(2.0f.thir, evaluator.evaluate(1.0f thirMul 2.0f))
            assertSuccess(0.5f.thir, evaluator.evaluate(1.0f thirDiv 2.0f))
            assertSuccess(1.0f.thir, evaluator.evaluate(1.0f thirMod 2.0f))
            assertSuccess((-1.0f).thir, evaluator.evaluate(1.0f.thirMinus))
            assertSuccess(1.0f.thir, evaluator.evaluate(1.0f.thirPlus))
            assertSuccess((-1.0f).thir, evaluator.evaluate(1.0f thirSub 2.0f))
            
            assertSuccess(false.thir, evaluator.evaluate(1.0 thirEq 2.0))
            assertSuccess(false.thir, evaluator.evaluate(1.0 thirGe 2.0))
            assertSuccess(false.thir, evaluator.evaluate(1.0 thirGt 2.0))
            assertSuccess(true.thir, evaluator.evaluate(1.0 thirLe 2.0))
            assertSuccess(true.thir, evaluator.evaluate(1.0 thirLt 2.0))
            assertSuccess(true.thir, evaluator.evaluate(1.0 thirNe 2.0))
            assertSuccess(3.0.thir, evaluator.evaluate(1.0 thirAdd 2.0))
            assertSuccess(0.5.thir, evaluator.evaluate(1.0 thirDiv 2.0))
            assertSuccess(1.0.thir, evaluator.evaluate(1.0 thirMod 2.0))
            assertSuccess(2.0.thir, evaluator.evaluate(1.0 thirMul 2.0))
            assertSuccess((-1.0).thir, evaluator.evaluate(1.0.thirMinus))
            assertSuccess(1.0.thir, evaluator.evaluate(1.0.thirPlus))
            assertSuccess((-1.0).thir, evaluator.evaluate(1.0 thirSub 2.0))
            
            assertSuccess(false.thir, evaluator.evaluate("foo" thirEq "bar"))
            assertSuccess(true.thir, evaluator.evaluate("foo" thirNe "bar"))
            assertSuccess("foobar".thir, evaluator.evaluate("foo" thirAdd "bar"))
            
            assertSuccess(null, evaluator.evaluate(Builtin.STR_PRINTLN.thirLoad().thirCall("testing-evaluator".thir)))
        }
        
        @Test
        fun `Given catch, when evaluating, then correct`()
        {
            val function = thirFunOf(
                errorKind = ThirKind.Value(ThirType.Int32),
                statements = listOf(0.thir.returnError),
            ).declare(env)
            val call = function.thirLoad().thirCall()
            
            assertSuccess(1.thir, evaluator.evaluate(call thirCatch 1.thir))
            assertFailure(EvaluatorReturnError(2.thir), tryCatch { evaluator.evaluate(call thirCatchError 2.thir) })
            assertFailure(EvaluatorReturnValue(3.thir), tryCatch { evaluator.evaluate(call thirCatchValue 3.thir) })
        }
        
        @Test
        fun `Given load, when evaluating, then correct`()
        {
            val variable = thirVarOf().declare(env)
            val structure = thirStructOf().declare(env)
            
            globals[variable.id] = 42.thir
    
            assertSuccess(42.thir, evaluator.evaluate(variable.thirLoad()))
            assertSuccess(structure.thirLoad(), evaluator.evaluate(structure.thirLoad()))
        }
        
        @Test
        fun `Given field, when evaluating, then correct`()
        {
            val field = thirFieldOf().declare(env)
            val structure = thirStructOf(fieldIds = listOf(field.id)).declare(env)
            val instance = ThirExpression.Instance(
                fields = mutableMapOf(field.id to 42.thir),
                valueKind = ThirKind.Value(ThirType.Structure(structure.id, emptyList())),
            )
            
            globals[structure.id] = instance
            
            assertSuccess(42.thir, evaluator.evaluate(instance.thirField(field)))
        }
        
        @Test
        fun `Given custom function invocations, when evaluating, then correct`()
        {
            val nothing = thirFunOf(
                valueKind = ThirKind.Nothing,
                errorKind = ThirKind.Nothing,
                statements = listOf(ThirStatement.Return),
            ).declare(env)
            val value = thirFunOf(
                valueKind = ThirKind.Value(ThirType.Int32),
                errorKind = ThirKind.Nothing,
                statements = listOf(1.thir.returnValue),
            ).declare(env)
            val error = thirFunOf(
                valueKind = ThirKind.Nothing,
                errorKind = ThirKind.Value(ThirType.Int32),
                statements = listOf(2.thir.returnError),
            ).declare(env)
            
            assertSuccess(null, evaluator.evaluate(nothing.thirLoad().thirCall()))
            assertSuccess(1.thir, evaluator.evaluate(value.thirLoad().thirCall()))
            assertFailure(2.thir, evaluator.evaluate(error.thirLoad().thirCall()))
        }
        
        @Test
        fun `Given structure without constructor parameters, when evaluating, then correct`()
        {
            val structure = thirStructOf(ctorEntryIds = emptyList(), fieldIds = emptyList()).declare(env)
            val expected = ThirExpression.Instance(
                fields = mutableMapOf(),
                valueKind = ThirKind.Value(ThirType.Structure(structure.id, emptyList())),
            )
            
            assertSuccess(expected, evaluator.evaluate(structure.thirLoad().thirCall()))
        }
        
        @Test
        fun `Given structure with constructor parameters, when evaluating, then correct`()
        {
            val parameter = thirParamOf().declare(env)
            val field = thirFieldOf(default = parameter.thirLoad()).declare(env)
            val structure = thirStructOf(ctorEntryIds = listOf(parameter.id), fieldIds = listOf(field.id)).declare(env)
            val instance = ThirExpression.Instance(
                fields = mutableMapOf(field.id to 42.thir),
                valueKind = ThirKind.Value(ThirType.Structure(structure.id, emptyList())),
            )
            
            assertSuccess(instance, evaluator.evaluate(structure.thirLoad().thirCall(42.thir)))
        }
    }
    
    @Nested
    inner class Statements
    {
        @Test
        fun `Given assignment statements, when executing, then correct`()
        {
            val variable = thirVarOf(kind = ThirKind.Value(ThirType.Int32)).declare(env)
            
            evaluator.execute(variable.thirLoad() thirAssign 42.thir)
            
            assertSuccess(42.thir, evaluator.evaluate(variable.thirLoad()))
        }
        
        @Test
        fun `Given evaluate statements, when executing, then correct`()
        {
            val function = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Nothing).declare(env)
            
            evaluator.execute(function.thirLoad().thirCall().thirEval)
        }
        
        @Test
        fun `Given if statements, when executing, then correct`()
        {
            val predicate = thirVarOf(kind = ThirKind.Value(ThirType.Bool)).declare(env)
            val variable = thirVarOf(kind = ThirKind.Value(ThirType.Int32)).declare(env)
            
            val statement = predicate.thirLoad().thirIf(
                success = listOf(variable.thirLoad() thirAssign 1.thir),
                failure = listOf(variable.thirLoad() thirAssign 2.thir),
            )
            
            evaluator.execute(predicate.thirLoad() thirAssign false.thir)
            evaluator.execute(statement)
            
            assertSuccess(2.thir, evaluator.evaluate(variable.thirLoad()))
        }
        
        @Test
        fun `Given return statements, when executing, then correct`()
        {
            assertThrows<EvaluatorReturn> { evaluator.execute(ThirStatement.Return) }
            assertThrows<EvaluatorReturnError> { evaluator.execute(0.thir.returnError) }
            assertThrows<EvaluatorReturnValue> { evaluator.execute(0.thir.returnValue) }
        }
        
        @Test
        fun `Given while statements, when executing, then correct`()
        {
            val variable = thirVarOf(
                kind = ThirKind.Value(ThirType.Int32),
                assignability = Assignability.ASSIGNABLE,
            ).declare(env)
            
            val predicate = Builtin.INT32_LT.thirLoad().thirCall(variable.thirLoad(), 3.thir)
            val increment = Builtin.INT32_ADD.thirLoad().thirCall(variable.thirLoad(), 1.thir)
            val statement = predicate.thirWhile(statements = listOf(variable.thirLoad() thirAssign increment))
            
            evaluator.execute(variable.thirLoad() thirAssign 0.thir)
            evaluator.execute(statement)
            
            assertSuccess(3.thir, evaluator.evaluate(variable.thirLoad()))
        }
    }
}
