package com.github.derg.transpiler.phases.parser

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class TestParser
{
    @Test
    fun `Given empty segment, when parsing, then correctly parsed`()
    {
        val expected = segmentOf(module = null, imports = emptySet(), statements = emptyList())
        
        assertEquals(expected, parse(""))
    }
    
    @Test
    fun `Given populated segment, when parsing, then correctly parsed`()
    {
        val variable = varOf("foo", 42.toExp("i8"))
        val expected = segmentOf(module = null, imports = emptySet(), statements = listOf(variable))
        
        assertEquals(expected, parse("val foo = 42i8"))
    }
}
