package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

class TestParser
{
    @Test
    fun `Given empty segment, when parsing, then correctly parsed`()
    {
        val expected = astSegmentOf(module = null, imports = emptyList(), statements = emptyList())
        
        assertSuccess(expected, parse(""))
    }
    
    @Test
    fun `Given populated segment, when parsing, then correctly parsed`()
    {
        val variable = astConstOf("foo", type = "Int", value = 42)
        val expected = astSegmentOf(module = null, imports = emptyList(), statements = listOf(variable))
        
        assertSuccess(expected, parse("val foo: Int = 42"))
    }
    
    @Test
    fun `Given syntax error, when parsing, then correct error`()
    {
        val expected = ParseError.UnexpectedToken(Keyword(Symbol.COMMA))
        
        assertFailure(expected, parse("use foo,"))
    }
}
