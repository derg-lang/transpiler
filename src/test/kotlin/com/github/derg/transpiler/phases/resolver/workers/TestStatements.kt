package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestAssignDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Test
    fun `Given assignable variable, when processing, then successful`()
    {
        val variable = thirVarOf(assignability = Assignability.ASSIGNABLE).register(scope).declare(env)
        
        val worker = AssignDefiner(evaluator, variable.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = variable.thirLoad() thirAssign 0.thir
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given final variable, when processing, then error`()
    {
        val variable = thirVarOf(assignability = Assignability.FINAL).register(scope).declare(env)
        
        val worker = AssignDefiner(evaluator, variable.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.VariableNotAssignable(variable.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given reference variable, when processing, then error`()
    {
        val variable = thirVarOf(assignability = Assignability.REFERENCE).register(scope).declare(env)
        
        val worker = AssignDefiner(evaluator, variable.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.VariableNotAssignable(variable.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given function, when processing, then error`()
    {
        val function = thirFunOf().register(scope).declare(env)
        
        val worker = AssignDefiner(evaluator, function.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.SymbolNotAssignable(function.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given parameter, when processing, then error`()
    {
        val parameter = thirParamOf().register(scope).declare(env)
        
        val worker = AssignDefiner(evaluator, parameter.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.SymbolNotAssignable(parameter.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given structure, when processing, then error`()
    {
        val structure = thirStructOf().register(scope).declare(env)
        
        val worker = AssignDefiner(evaluator, structure.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.SymbolNotAssignable(structure.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given type parameter, when processing, then error`()
    {
        val parameter = thirTypeParamOf().register(scope).declare(env)
        
        val worker = AssignDefiner(evaluator, parameter.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.SymbolNotAssignable(parameter.name)
        
        assertFailure(expected, worker.process())
    }
}

class TestEvaluateDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Test
    fun `Given value type, when processing, then error`()
    {
        val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Int32), errorKind = ThirKind.Nothing).register(scope).declare(env)
        
        val worker = EvaluateDefiner(evaluator, function.name.hirIdent().hirCall().hirEval, env, scope)
        val expected = Outcome.EvaluationHasValue(ThirKind.Value(ThirType.Int32))
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given error type, when processing, then error`()
    {
        val function = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Value(ThirType.Int32)).register(scope).declare(env)
        
        val worker = EvaluateDefiner(evaluator, function.name.hirIdent().hirCall().hirEval, env, scope)
        val expected = Outcome.EvaluationHasError(ThirKind.Value(ThirType.Int32))
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given neither value nor error type, when processing, then successful`()
    {
        val function = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Nothing).register(scope).declare(env)
        
        val worker = EvaluateDefiner(evaluator, function.name.hirIdent().hirCall().hirEval, env, scope)
        val expected = function.thirLoad().thirCall().thirEval
        
        assertSuccess(expected, worker.process())
    }
}

class TestIfDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Nested
    inner class `Predicate cases`
    {
        @Test
        fun `Given valid predicate value type, when processing, then success`()
        {
            val worker = IfDefiner(evaluator, true.hirIf(), env, scope)
            val expected = true.thir.thirIf()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given invalid predicate value type, when processing, then error`()
        {
            val worker = IfDefiner(evaluator, 0.hirIf(), env, scope)
            val expected = Outcome.PredicateWrongType(ThirType.Int32)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given no predicate value type, when processing, then error`()
        {
            val function = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Nothing).register(scope).declare(env)
            
            val worker = IfDefiner(evaluator, function.name.hirIdent().hirCall().hirIf(), env, scope)
            val expected = Outcome.PredicateWrongKind(ThirKind.Nothing)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given any predicate error type, when processing, then error`()
        {
            val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Bool), errorKind = ThirKind.Value(ThirType.Int32)).register(scope).declare(env)
            
            val worker = IfDefiner(evaluator, function.name.hirIdent().hirCall().hirIf(), env, scope)
            val expected = Outcome.PredicateHasError(ThirKind.Value(ThirType.Int32))
            
            assertFailure(expected, worker.process())
        }
    }
    
    @Nested
    inner class `Branch cases`
    {
        @Test
        fun `Given valid success branch statement, when processing, then success`()
        {
            val worker = IfDefiner(evaluator, true.hirIf(success = listOf(0.hirReturnValue)), env, scope)
            val expected = true.thir.thirIf(success = listOf(0.thir.returnValue))
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given valid failure branch statement, when processing, then success`()
        {
            val worker = IfDefiner(evaluator, true.hirIf(failure = listOf(0.hirReturnValue)), env, scope)
            val expected = true.thir.thirIf(failure = listOf(0.thir.returnValue))
            
            assertSuccess(expected, worker.process())
        }
    }
}

class TestReturnErrorDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Test
    fun `Given valid expression, when processing, then success`()
    {
        val worker = ReturnErrorDefiner(evaluator, 0.hirReturnError, env, scope)
        val expected = 0.thir.returnError
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given any error type, when processing, then error`()
    {
        val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Bool), errorKind = ThirKind.Value(ThirType.Int32)).register(scope).declare(env)
        
        val worker = ReturnErrorDefiner(evaluator, function.name.hirIdent().hirCall().hirReturnError, env, scope)
        val expected = Outcome.ReturnHasError(ThirKind.Value(ThirType.Int32))
        
        assertFailure(expected, worker.process())
    }
}

class TestReturnValueDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Test
    fun `Given valid expression, when processing, then success`()
    {
        val worker = ReturnValueDefiner(evaluator, 0.hirReturnValue, env, scope)
        val expected = 0.thir.returnValue
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given any error type, when processing, then error`()
    {
        val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Bool), errorKind = ThirKind.Value(ThirType.Int32)).register(scope).declare(env)
        
        val worker = ReturnValueDefiner(evaluator, function.name.hirIdent().hirCall().hirReturnValue, env, scope)
        val expected = Outcome.ReturnHasError(ThirKind.Value(ThirType.Int32))
        
        assertFailure(expected, worker.process())
    }
}

class TestVariableDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Test
    fun `Given both type and value, when processing, then declared and defined`()
    {
        val input = hirVarOf(kind = INT32_TYPE_NAME.hirIdent().type.kind, value = 1.hir)
        val variable = thirVarOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Int32), value = 1.thir)
        val expected = variable.thirLoad() thirAssign 1.thir
        
        assertSuccess(expected, VariableDefiner(evaluator, input, env, scope).process())
        assertEquals(variable, env.declarations[input.id])
        assertTrue(variable.id in scope.find(variable.name))
    }
    
    @Test
    fun `Given only value, when processing, then declared and defined`()
    {
        val input = hirVarOf(kind = null, value = 1.hir)
        val variable = thirVarOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Int32), value = 1.thir)
        val expected = variable.thirLoad() thirAssign 1.thir
        
        assertSuccess(expected, VariableDefiner(evaluator, input, env, scope).process())
        assertEquals(variable, env.declarations[input.id])
        assertTrue(variable.id in scope.find(variable.name))
    }
    
    @Test
    fun `Given mismatched type and value, when processing, then error`()
    {
        val input = hirVarOf(kind = INT32_TYPE_NAME.hirIdent().type.kind, value = true.hir)
        val expected = Outcome.BindingWrongType(ThirType.Int32, ThirType.Bool)
        
        assertFailure(expected, VariableDefiner(evaluator, input, env, scope).process())
    }
}

class TestWhileDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Nested
    inner class `Predicate cases`
    {
        @Test
        fun `Given valid predicate value type, when processing, then success`()
        {
            val worker = WhileDefiner(evaluator, true.hirWhile(), env, scope)
            val expected = true.thir.thirWhile()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given invalid predicate value type, when processing, then error`()
        {
            val worker = WhileDefiner(evaluator, 0.hirWhile(), env, scope)
            val expected = Outcome.PredicateWrongType(ThirType.Int32)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given no predicate value type, when processing, then error`()
        {
            val function = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Nothing).register(scope).declare(env)
            
            val worker = WhileDefiner(evaluator, function.name.hirIdent().hirCall().hirWhile(), env, scope)
            val expected = Outcome.PredicateWrongKind(ThirKind.Nothing)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given any predicate error type, when processing, then error`()
        {
            val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Bool), errorKind = ThirKind.Value(ThirType.Int32)).register(scope).declare(env)
            
            val worker = WhileDefiner(evaluator, function.name.hirIdent().hirCall().hirWhile(), env, scope)
            val expected = Outcome.PredicateHasError(ThirKind.Value(ThirType.Int32))
            
            assertFailure(expected, worker.process())
        }
    }
    
    @Nested
    inner class `Statement cases`
    {
        @Test
        fun `Given valid statement, when processing, then success`()
        {
            val worker = WhileDefiner(evaluator, true.hirWhile(statements = listOf(0.hirReturnValue)), env, scope)
            val expected = true.thir.thirWhile(statements = listOf(0.thir.returnValue))
            
            assertSuccess(expected, worker.process())
        }
    }
}
