package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestConstDefiner
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
    fun `Given both type and value, when processing, then declared then defined`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirConstOf(kind = INT32_TYPE_NAME.hirIdent().type.kind, value = 1.hir)
        val expected = thirConstOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Int32), value = 1.thir)
        val worker = ConstDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given only value, when processing, then declared then defined`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirConstOf(kind = null, value = 1.hir)
        val expected = thirConstOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Int32), value = 1.thir)
        val worker = ConstDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given mismatched type and value, when processing, then error`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirConstOf(kind = INT32_TYPE_NAME.hirIdent().type.kind, value = true.hir)
        val expected = Outcome.BindingWrongType(ThirType.Int32, ThirType.Bool)
        val worker = ConstDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertFailure(expected, worker.process())
    }
}

class TestParameterDefiner
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
    fun `Given both type and default, when processing, then declared then defined`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirParamOf(kind = INT32_TYPE_NAME.hirIdent().type.kind, default = 1.hir)
        val expected = thirParamOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Int32), default = 1.thir)
        val worker = ParameterDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given only type, when processing, then declared then defined`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirParamOf(kind = INT32_TYPE_NAME.hirIdent().type.kind, default = null)
        val expected = thirParamOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Int32), default = null)
        val worker = ParameterDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given mismatched type and default, when processing, then error`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirParamOf(kind = INT32_TYPE_NAME.hirIdent().type.kind, default = true.hir)
        val expected = Outcome.BindingWrongType(ThirType.Int32, ThirType.Bool)
        val worker = ParameterDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertFailure(expected, worker.process())
    }
}

class TestFunctionDefiner
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
    fun `Given empty, when processing, then declared then defined`()
    {
        val input = hirFunOf()
        val expected = thirFunOf(id = input.id, name = input.name)
        val worker = FunctionDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given value type, when processing, then declared then defined`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirFunOf(valueKind = INT32_TYPE_NAME.hirIdent().type.kind)
        val expected = thirFunOf(id = input.id, name = input.name, valueKind = ThirKind.Value(ThirType.Int32))
        val worker = FunctionDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given error type, when processing, then declared then defined`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirFunOf(errorKind = INT32_TYPE_NAME.hirIdent().type.kind)
        val expected = thirFunOf(id = input.id, name = input.name, errorKind = ThirKind.Value(ThirType.Int32))
        val worker = FunctionDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given statement, when processing, then declared then defined`()
    {
        val input = hirFunOf(statements = listOf(true.hirIf()))
        val expected = thirFunOf(id = input.id, name = input.name, statements = listOf(true.thir.thirIf()))
        val worker = FunctionDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given parameter, when processing, then spawned before declared and defined`()
    {
        val parameter = hirParamOf()
        val input = hirFunOf(parameters = listOf(parameter))
        val expected = thirFunOf(id = input.id, name = input.name, parameterIds = listOf(parameter.id))
        val worker = FunctionDefiner(input, env, scope)
        
        assertIs<Phase.Spawn>(worker.process().valueOrDie())
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
}

class TestStructureDefiner
{
    private val env = Environment()
    private val scope = Scope()
    
    @Test
    fun `Given empty, when processing, then declared then defined`()
    {
        val input = hirStructOf()
        val expected = thirStructOf(id = input.id, name = input.name)
        val worker = StructureDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given field, when processing, then spawned before declared and defined`()
    {
        val field = hirFieldOf()
        val input = hirStructOf(fields = listOf(field))
        val expected = thirStructOf(id = input.id, name = input.name, fieldIds = listOf(field.id))
        val worker = StructureDefiner(input, env, scope)
        
        assertIs<Phase.Spawn>(worker.process().valueOrDie())
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected.copy(def = null), env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
}

class TestVariableDefiner
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
    fun `Given both type and value, when processing, then declared and defined`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirVarOf(kind = INT32_TYPE_NAME.hirIdent().type.kind, value = 1.hir)
        val expected = thirVarOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Int32), value = 1.thir)
        val worker = VariableDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected, env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given only value, when processing, then declared and defined`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirVarOf(kind = null, value = 1.hir)
        val expected = thirVarOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Int32), value = 1.thir)
        val worker = VariableDefiner(input, env, scope)
        
        assertSuccess(Phase.Declared, worker.process())
        assertEquals(expected, env.declarations[input.id])
        assertSuccess(Phase.Defined, worker.process())
        assertEquals(expected, env.declarations[input.id])
    }
    
    @Test
    fun `Given mismatched type and value, when processing, then error`()
    {
        Builtin.INT32.register().declare()
        
        val input = hirVarOf(kind = INT32_TYPE_NAME.hirIdent().type.kind, value = true.hir)
        val expected = Outcome.BindingWrongType(ThirType.Int32, ThirType.Bool)
        
        assertFailure(expected, VariableDefiner(input, env, scope).process())
    }
}
