package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.util.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestDefinerFunction
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val definer = DefinerFunction(symbols)
    
    @Test
    fun `Given valid function, when defining, then defined`()
    {
        val function = thirFunOf("fun")
        
        assertEquals(successOf(), definer(function, emptyList()))
        assertNotNull(function.scope)
    }
    
    @Test
    fun `Given parameters, when defining, then parameters registered`()
    {
        val param = thirParOf("foo", Builtin.BOOL)
        val function = thirFunOf("fun", params = listOf(param))
        
        assertEquals(successOf(), definer(function, emptyList()))
        assertEquals(listOf(param), function.scope.symbols.find(param.name))
    }
}

class TestDefinerType
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val definer = DefinerType(symbols)
}
