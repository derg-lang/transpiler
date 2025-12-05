package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestAssignDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    @Test
    fun `Given assignable variable, when processing, then successful`()
    {
        val variable = thirVarOf(assignability = Assignability.ASSIGNABLE).register(scope).declare(env)
        
        val worker = AssignDefiner(variable.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = variable.thirLoad() thirAssign 0.thir
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given final variable, when processing, then error`()
    {
        val variable = thirVarOf(assignability = Assignability.FINAL).register(scope).declare(env)
        
        val worker = AssignDefiner(variable.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.VariableNotAssignable(variable.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given reference variable, when processing, then error`()
    {
        val variable = thirVarOf(assignability = Assignability.REFERENCE).register(scope).declare(env)
        
        val worker = AssignDefiner(variable.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.VariableNotAssignable(variable.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given function, when processing, then error`()
    {
        val function = thirFunOf().register(scope).declare(env)
        
        val worker = AssignDefiner(function.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.SymbolNotAssignable(function.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given parameter, when processing, then error`()
    {
        val parameter = thirParamOf().register(scope).declare(env)
        
        val worker = AssignDefiner(parameter.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.SymbolNotAssignable(parameter.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given structure, when processing, then error`()
    {
        val structure = thirStructOf().register(scope).declare(env)
        
        val worker = AssignDefiner(structure.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.SymbolNotAssignable(structure.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given type parameter, when processing, then error`()
    {
        val parameter = thirTypeParamOf().register(scope).declare(env)
        
        val worker = AssignDefiner(parameter.name.hirIdent() hirAssign 0.hir, env, scope)
        val expected = Outcome.SymbolNotAssignable(parameter.name)
        
        assertFailure(expected, worker.process())
    }
}

class TestEvaluateDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    /**
     * Registers [this] declaration to the current scope.
     */
    private fun <Type : ThirDeclaration> Type.register(): Type =
        apply { scope.register(id, name) }
    
    /**
     * Declares that [this] declaration actually exists within the environment.
     */
    private fun <Type : ThirDeclaration> Type.declare(): Type =
        apply { env.declarations[id] = this }
    
    @Test
    fun `Given value type, when processing, then error`()
    {
        val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Int32), errorKind = ThirKind.Nothing).register().declare()
        
        val worker = EvaluateDefiner(function.name.hirIdent().hirCall().hirEval, env, scope)
        val expected = Outcome.EvaluationHasValue(ThirKind.Value(ThirType.Int32))
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given error type, when processing, then error`()
    {
        val function = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Value(ThirType.Int32)).register().declare()
        
        val worker = EvaluateDefiner(function.name.hirIdent().hirCall().hirEval, env, scope)
        val expected = Outcome.EvaluationHasError(ThirKind.Value(ThirType.Int32))
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given neither value nor error type, when processing, then successful`()
    {
        val function = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Nothing).register().declare()
        
        val worker = EvaluateDefiner(function.name.hirIdent().hirCall().hirEval, env, scope)
        val expected = function.thirLoad().thirCall().thirEval
        
        assertSuccess(expected, worker.process())
    }
}

class TestIfDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    /**
     * Registers [this] declaration to the current scope.
     */
    private fun <Type : ThirDeclaration> Type.register(): Type =
        apply { scope.register(id, name) }
    
    /**
     * Declares that [this] declaration actually exists within the environment.
     */
    private fun <Type : ThirDeclaration> Type.declare(): Type =
        apply { env.declarations[id] = this }
    
    @Nested
    inner class `Predicate cases`
    {
        @Test
        fun `Given valid predicate value type, when processing, then success`()
        {
            val worker = IfDefiner(true.hirIf(), env, scope)
            val expected = true.thir.thirIf()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given invalid predicate value type, when processing, then error`()
        {
            val worker = IfDefiner(0.hirIf(), env, scope)
            val expected = Outcome.PredicateWrongType(ThirType.Int32)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given no predicate value type, when processing, then error`()
        {
            val function = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Nothing).register().declare()
            
            val worker = IfDefiner(function.name.hirIdent().hirCall().hirIf(), env, scope)
            val expected = Outcome.PredicateWrongKind(ThirKind.Nothing)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given any predicate error type, when processing, then error`()
        {
            val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Bool), errorKind = ThirKind.Value(ThirType.Int32)).register().declare()
            
            val worker = IfDefiner(function.name.hirIdent().hirCall().hirIf(), env, scope)
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
            val worker = IfDefiner(true.hirIf(success = listOf(0.hirReturnValue)), env, scope)
            val expected = true.thir.thirIf(success = listOf(0.thir.returnValue))
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given valid failure branch statement, when processing, then success`()
        {
            val worker = IfDefiner(true.hirIf(failure = listOf(0.hirReturnValue)), env, scope)
            val expected = true.thir.thirIf(failure = listOf(0.thir.returnValue))
            
            assertSuccess(expected, worker.process())
        }
    }
}

class TestInitializeDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    /**
     * Declares that [this] declaration actually exists within the environment.
     */
    private fun <Type : ThirDeclaration> Type.declare(): Type =
        apply { env.declarations[id] = this }
    
    @Test
    fun `Given unknown variable, when processing, then error`()
    {
        val input = hirVarOf(name = "whatever")
        val expected = Outcome.RequireDefinition(setOf(input.id))
        
        assertFailure(expected, InitializeDefiner(input, env, scope).process())
    }
    
    @Test
    fun `Given defined variable, when processing, then registered in scope`()
    {
        val input = hirVarOf(kind = Builtin.BOOL.name.hirIdent().type.kind, value = 0.hir)
        val variable = thirVarOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Int32), value = 0.thir).declare()
        val expected = variable.thirLoad() thirAssign 0.thir
        
        assertFalse(variable.id in scope.find(input.name))
        assertSuccess(expected, InitializeDefiner(input, env, scope).process())
        assertTrue(variable.id in scope.find(variable.name))
    }
}

class TestReturnErrorDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    /**
     * Registers [this] declaration to the current scope.
     */
    private fun <Type : ThirDeclaration> Type.register(): Type =
        apply { scope.register(id, name) }
    
    /**
     * Declares that [this] declaration actually exists within the environment.
     */
    private fun <Type : ThirDeclaration> Type.declare(): Type =
        apply { env.declarations[id] = this }
    
    @Test
    fun `Given valid expression, when processing, then success`()
    {
        val worker = ReturnErrorDefiner(0.hirReturnError, env, scope)
        val expected = 0.thir.returnError
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given any error type, when processing, then error`()
    {
        val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Bool), errorKind = ThirKind.Value(ThirType.Int32)).register().declare()
        
        val worker = ReturnErrorDefiner(function.name.hirIdent().hirCall().hirReturnError, env, scope)
        val expected = Outcome.ReturnHasError(ThirKind.Value(ThirType.Int32))
        
        assertFailure(expected, worker.process())
    }
}

class TestReturnValueDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    /**
     * Registers [this] declaration to the current scope.
     */
    private fun <Type : ThirDeclaration> Type.register(): Type =
        apply { scope.register(id, name) }
    
    /**
     * Declares that [this] declaration actually exists within the environment.
     */
    private fun <Type : ThirDeclaration> Type.declare(): Type =
        apply { env.declarations[id] = this }
    
    @Test
    fun `Given valid expression, when processing, then success`()
    {
        val worker = ReturnValueDefiner(0.hirReturnValue, env, scope)
        val expected = 0.thir.returnValue
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given any error type, when processing, then error`()
    {
        val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Bool), errorKind = ThirKind.Value(ThirType.Int32)).register().declare()
        
        val worker = ReturnValueDefiner(function.name.hirIdent().hirCall().hirReturnValue, env, scope)
        val expected = Outcome.ReturnHasError(ThirKind.Value(ThirType.Int32))
        
        assertFailure(expected, worker.process())
    }
}

class TestWhileDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    /**
     * Registers [this] declaration to the current scope.
     */
    private fun <Type : ThirDeclaration> Type.register(): Type =
        apply { scope.register(id, name) }
    
    /**
     * Declares that [this] declaration actually exists within the environment.
     */
    private fun <Type : ThirDeclaration> Type.declare(): Type =
        apply { env.declarations[id] = this }
    
    @Nested
    inner class `Predicate cases`
    {
        @Test
        fun `Given valid predicate value type, when processing, then success`()
        {
            val worker = WhileDefiner(true.hirWhile(), env, scope)
            val expected = true.thir.thirWhile()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given invalid predicate value type, when processing, then error`()
        {
            val worker = WhileDefiner(0.hirWhile(), env, scope)
            val expected = Outcome.PredicateWrongType(ThirType.Int32)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given no predicate value type, when processing, then error`()
        {
            val function = thirFunOf(valueKind = ThirKind.Nothing, errorKind = ThirKind.Nothing).register().declare()
            
            val worker = WhileDefiner(function.name.hirIdent().hirCall().hirWhile(), env, scope)
            val expected = Outcome.PredicateWrongKind(ThirKind.Nothing)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given any predicate error type, when processing, then error`()
        {
            val function = thirFunOf(valueKind = ThirKind.Value(ThirType.Bool), errorKind = ThirKind.Value(ThirType.Int32)).register().declare()
            
            val worker = WhileDefiner(function.name.hirIdent().hirCall().hirWhile(), env, scope)
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
            val worker = WhileDefiner(true.hirWhile(statements = listOf(0.hirReturnValue)), env, scope)
            val expected = true.thir.thirWhile(statements = listOf(0.thir.returnValue))
            
            assertSuccess(expected, worker.process())
        }
    }
}
