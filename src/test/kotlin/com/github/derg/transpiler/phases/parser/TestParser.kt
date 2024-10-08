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
        val expected = astSegmentOf(imports = emptyList(), statements = emptyList())
        
        assertSuccess(expected, parse(""))
    }
    
    @Test
    fun `Given import, when parsing, then correctly parsed`()
    {
        val expected = astSegmentOf(imports = listOf("foo"))
        
        assertSuccess(expected, parse("use foo"))
    }
    
    @Test
    fun `Given statement, when parsing, then correctly parsed`()
    {
        val variable = astConstOf("foo", type = "Int", value = 42)
        val expected = astSegmentOf(statements = listOf(variable))
        
        assertSuccess(expected, parse("val foo: Int = 42"))
    }
    
    @Test
    fun `Given syntax error, when parsing, then correct error`()
    {
        val expected = ParseError.UnexpectedToken(Keyword(Symbol.COMMA))
        
        assertFailure(expected, parse("use foo,"))
    }
}
