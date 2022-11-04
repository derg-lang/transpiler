package com.github.derg.transpiler.parser

import com.github.derg.transpiler.ast.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class TestParser
{
    @Test
    fun `Given empty segment, when parsing, then correctly parsed`()
    {
        val expected = Segment(module = null, imports = emptySet(), definitions = emptyList())
        
        assertEquals(expected, parse(""))
    }
    
    @Test
    fun `Given populated segment, when parsing, then correctly parsed`()
    {
        val variable = Variable("foo", null, Value.Real(42.toBigDecimal(), "i8"), Visibility.PRIVATE, Mutability.VALUE)
        val expected = Segment(module = null, imports = emptySet(), definitions = listOf(variable))
        
        assertEquals(expected, parse("val foo = 42i8"))
    }
}
