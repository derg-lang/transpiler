package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestResolver
{
    private val resolver = Resolver(Builtin.environment, Builtin.scope)
    
    @Test
    fun `Hmm, what to do`()
    {
        val input = hirConstOf(kind = BOOL_TYPE_NAME.hirIdent().type.kind, value = true.hir)
        val segment = hirSegmentOf(constants = listOf(input))
        val expected = thirConstOf(id = input.id, name = input.name, kind = ThirKind.Value(ThirType.Bool), value = true.thir)
        
        assertSuccess(Unit, resolver.resolve(segment))
        assertEquals(expected, Builtin.environment.declarations[input.id])
    }
}
