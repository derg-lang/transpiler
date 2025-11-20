package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

class TestTypeExpressionDefiner
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
    fun `Given unknown structure identifier, when processing, then error`()
    {
        val structure = thirStructOf()
        val worker = TypeExpressionDefiner(structure.name.hirIdent().type, env, scope)
        val expected = Outcome.UnknownIdentifier(structure.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given unregistered structure identifier, when processing, then error`()
    {
        val structure = thirStructOf().register()
        val worker = TypeExpressionDefiner(structure.name.hirIdent().type, env, scope)
        val expected = Outcome.RequireDeclaration(setOf(structure.id))
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given valid structure identifier, when processing, then success`()
    {
        val structure = thirStructOf().register().declare()
        val worker = TypeExpressionDefiner(structure.name.hirIdent().type, env, scope)
        val expected = ThirType.Structure(structure.id, emptyList())
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given invalid type expression, when processing, then error`()
    {
        val worker = TypeExpressionDefiner(0.hir.type, env, scope)
        val expected = Outcome.Unhandled("Expression 'Int32(raw=0)' evaluated to a non-type value 'Int32(raw=0)'")
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given const indirection to invalid expression, when processing, then error`()
    {
        val const = thirConstOf(kind = ThirKind.Value(ThirType.Bool), value = true.thir).register().declare()
        
        val worker = TypeExpressionDefiner(const.name.hirIdent().type, env, scope)
        val expected = Outcome.Unhandled("Expression 'Load(symbolId=${const.id}, valueKind=Value(type=Bool))' evaluated to a non-type value 'Bool(raw=true)'")
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given const indirection to valid expression, when processing, then success`()
    {
        val structure = thirStructOf().register().declare()
        val const = thirConstOf(kind = ThirKind.Type, value = structure.thirLoad()).register().declare()
        
        val worker = TypeExpressionDefiner(const.name.hirIdent().type, env, scope)
        val expected = ThirType.Structure(structure.id, emptyList())
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given const indirection to undefined expression, when processing, then error`()
    {
        val const = thirConstOf(kind = ThirKind.Type).copy(def = null).register().declare()
        
        val worker = TypeExpressionDefiner(const.name.hirIdent().type, env, scope)
        val expected = Outcome.RequireDefinition(setOf(const.id))
    
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given type parameter, when processing, then success`()
    {
        val generic = thirParamOf().register().declare()
        
        val worker = TypeExpressionDefiner(generic.name.hirIdent().type, env, scope)
        val expected = ThirType.TypeParameterRef(generic.id)
        
        assertSuccess(expected, worker.process())
    }
}
