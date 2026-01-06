package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestResolver
{
    private val env = Builtin.generateEnvironment()
    private val scope = Builtin.generateScope()
    private val stack = Stack()
    
    private val evaluator = Evaluator(env, stack)
    private val resolver = Resolver(env, scope, stack, evaluator)
    
    @Test
    fun `Hmm, what to do`()
    {
        val input = hirConstOf(kind = BOOL_TYPE_NAME.hirIdent().type.kind, value = true.hir)
        val segment = hirSegmentOf(constants = listOf(input))
        val module = hirModuleOf(segments = listOf(segment))
        val expected = thirConstOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Bool), value = true.thir)
        
        assertSuccess(Unit, resolver.resolve(module))
        assertEquals(expected, env.declarations[input.id])
    }
}
