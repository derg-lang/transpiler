package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

class TestTypeExpressionDefiner
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    private val evaluator = Evaluator(env, stack)
    
    @Test
    fun `Given unknown structure identifier, when processing, then error`()
    {
        val structure = thirStructOf()
        val worker = TypeExpressionDefiner(evaluator, structure.name.hirIdent().type, env, scope)
        val expected = Outcome.UnknownIdentifier(structure.name)
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given unregistered structure identifier, when processing, then error`()
    {
        val structure = thirStructOf().register(scope)
        val worker = TypeExpressionDefiner(evaluator, structure.name.hirIdent().type, env, scope)
        val expected = Outcome.RequireDeclaration(setOf(structure.id))
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given valid structure identifier, when processing, then success`()
    {
        val structure = thirStructOf().register(scope).declare(env)
        val worker = TypeExpressionDefiner(evaluator, structure.name.hirIdent().type, env, scope)
        val expected = ThirType.Structure(structure.id, emptyList())
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given invalid type expression, when processing, then error`()
    {
        val worker = TypeExpressionDefiner(evaluator, 0.hir.type, env, scope)
        val expected = Outcome.Unhandled("Expression 'Int32(raw=0)' evaluated to a non-type value 'Int32(raw=0)'")
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given const indirection to invalid expression, when processing, then error`()
    {
        val const = thirConstOf(kind = ThirKind.Value(ThirType.Bool), value = true.thir).register(scope).declare(env).push(stack)
        
        val worker = TypeExpressionDefiner(evaluator, const.name.hirIdent().type, env, scope)
        val expected = Outcome.Unhandled("Expression 'Load(symbolId=${const.id}, valueKind=Value(type=Bool))' evaluated to a non-type value 'Bool(raw=true)'")
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given const indirection to valid expression, when processing, then success`()
    {
        val structure = thirStructOf().register(scope).declare(env)
        val value = ThirExpression.Type(ThirType.Structure(structure.id, emptyList()))
        val const = thirConstOf(kind = ThirKind.Type, value = value).register(scope).declare(env).push(stack)
        
        val worker = TypeExpressionDefiner(evaluator, const.name.hirIdent().type, env, scope)
        val expected = ThirType.Structure(structure.id, emptyList())
        
        assertSuccess(expected, worker.process())
    }
    
    @Test
    fun `Given const indirection to undefined expression, when processing, then error`()
    {
        val const = thirConstOf(kind = ThirKind.Type).copy(def = null).register(scope).declare(env)
        
        val worker = TypeExpressionDefiner(evaluator, const.name.hirIdent().type, env, scope)
        val expected = Outcome.RequireDefinition(setOf(const.id))
        
        assertFailure(expected, worker.process())
    }
    
    @Test
    fun `Given type parameter, when processing, then success`()
    {
        val generic = thirParamOf().register(scope).declare(env)
        
        val worker = TypeExpressionDefiner(evaluator, generic.name.hirIdent().type, env, scope)
        val expected = ThirType.TypeParameterRef(generic.id)
        
        assertSuccess(expected, worker.process())
    }
}
