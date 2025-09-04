package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

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
        val function = thirFunOf(valueType = ThirType.Int32, errorType = ThirType.Void).register().declare()
        
        val worker = EvaluateDefiner(function.name.hirIdent().hirCall().hirEval, env, scope)
        val expected = Outcome.MismatchedType(expected = ThirType.Void, received = ThirType.Int32)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given error type, when processing, then error`()
    {
        val function = thirFunOf(valueType = ThirType.Void, errorType = ThirType.Int32).register().declare()
        
        val worker = EvaluateDefiner(function.name.hirIdent().hirCall().hirEval, env, scope)
        val expected = Outcome.MismatchedType(expected = ThirType.Void, received = ThirType.Int32)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given neither value nor error type, when processing, then successful`()
    {
        val function = thirFunOf(valueType = ThirType.Void, errorType = ThirType.Void).register().declare()
        
        val worker = EvaluateDefiner(function.name.hirIdent().hirCall().hirEval, env, scope)
        val expected = function.thirCall().thirEval
        
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
            val expected = true.thirIf()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given invalid predicate value type, when processing, then error`()
        {
            val worker = IfDefiner(0.hirIf(), env, scope)
            val expected = Outcome.MismatchedType(expected = ThirType.Bool, received = ThirType.Int32)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given no predicate value type, when processing, then error`()
        {
            val function = thirFunOf(valueType = ThirType.Void, errorType = ThirType.Void).register().declare()
            
            val worker = IfDefiner(function.name.hirIdent().hirCall().hirIf(), env, scope)
            val expected = Outcome.MismatchedType(expected = ThirType.Bool, received = ThirType.Void)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given any predicate error type, when processing, then error`()
        {
            val function = thirFunOf(valueType = ThirType.Bool, errorType = ThirType.Int32).register().declare()
            
            val worker = IfDefiner(function.name.hirIdent().hirCall().hirIf(), env, scope)
            val expected = Outcome.MismatchedType(expected = ThirType.Void, received = ThirType.Int32)
            
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
            val expected = true.thirIf(success = listOf(0.thirReturnValue))
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given valid failure branch statement, when processing, then success`()
        {
            val worker = IfDefiner(true.hirIf(failure = listOf(0.hirReturnValue)), env, scope)
            val expected = true.thirIf(failure = listOf(0.thirReturnValue))
            
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
        val input = hirVarOf(type = Builtin.BOOL.name.hirIdent().hirType(), value = 0.hir)
        val variable = thirVarOf(id = input.id, name = input.name, type = ThirType.Int32, value = 0.thir).declare()
        val expected = variable.thirIdent() thirAssign 0
        
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
        val expected = 0.thirReturnError
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given any error type, when processing, then error`()
    {
        val function = thirFunOf(valueType = ThirType.Bool, errorType = ThirType.Int32).register().declare()
        
        val worker = ReturnErrorDefiner(function.name.hirIdent().hirCall().hirReturnError, env, scope)
        val expected = Outcome.MismatchedType(expected = ThirType.Void, received = ThirType.Int32)
        
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
        val expected = 0.thirReturnValue
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given any error type, when processing, then error`()
    {
        val function = thirFunOf(valueType = ThirType.Bool, errorType = ThirType.Int32).register().declare()
        
        val worker = ReturnValueDefiner(function.name.hirIdent().hirCall().hirReturnValue, env, scope)
        val expected = Outcome.MismatchedType(expected = ThirType.Void, received = ThirType.Int32)
        
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
            val expected = true.thirWhile()
            
            assertSuccess(expected, worker.process())
        }
        
        @Test
        fun `Given invalid predicate value type, when processing, then error`()
        {
            val worker = WhileDefiner(0.hirWhile(), env, scope)
            val expected = Outcome.MismatchedType(expected = ThirType.Bool, received = ThirType.Int32)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given no predicate value type, when processing, then error`()
        {
            val function = thirFunOf(valueType = ThirType.Void, errorType = ThirType.Void).register().declare()
            
            val worker = WhileDefiner(function.name.hirIdent().hirCall().hirWhile(), env, scope)
            val expected = Outcome.MismatchedType(expected = ThirType.Bool, received = ThirType.Void)
            
            assertFailure(expected, worker.process())
        }
        
        @Test
        fun `Given any predicate error type, when processing, then error`()
        {
            val function = thirFunOf(valueType = ThirType.Bool, errorType = ThirType.Int32).register().declare()
            
            val worker = WhileDefiner(function.name.hirIdent().hirCall().hirWhile(), env, scope)
            val expected = Outcome.MismatchedType(expected = ThirType.Void, received = ThirType.Int32)
            
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
            val expected = true.thirWhile(statements = listOf(0.thirReturnValue))
            
            assertSuccess(expected, worker.process())
        }
    }
}
