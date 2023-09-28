package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.ast.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestParser
{
    @Test
    fun `Given empty segment, when parsing, then correctly parsed`()
    {
        val expected = astSegmentOf(module = null, imports = emptyList(), statements = emptyList())
        
        assertEquals(expected, parse(""))
    }
    
    @Test
    fun `Given populated segment, when parsing, then correctly parsed`()
    {
        val variable = astVarOf("foo", 42)
        val expected = astSegmentOf(module = null, imports = emptyList(), statements = listOf(variable))
        
        assertEquals(expected, parse("val foo = 42"))
    }
}
