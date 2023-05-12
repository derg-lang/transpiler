package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.hir.Builtin
import com.github.derg.transpiler.source.hir.SymbolTable
import com.github.derg.transpiler.source.hir.hirFunOf
import com.github.derg.transpiler.source.hir.hirParOf
import com.github.derg.transpiler.util.successOf
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Test

class TestDefinerFunction
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val definer = DefinerFunction(symbols)
    
    @Test
    fun `Given valid function, when defining, then defined`()
    {
        val function = hirFunOf("fun")
        
        assertEquals(successOf(), definer(function, emptyList()))
        assertNotNull(function.scope)
    }
    
    @Test
    fun `Given parameters, when defining, then parameters registered`()
    {
        val param = hirParOf("foo", Builtin.BOOL)
        val function = hirFunOf("fun", params = listOf(param))
        
        assertEquals(successOf(), definer(function, emptyList()))
        assertEquals(listOf(param), function.scope.symbols.find(param.name))
    }
}

class TestDefinerType
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val definer = DefinerType(symbols)
}
