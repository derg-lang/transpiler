package com.github.derg.transpiler.parser

import com.github.derg.transpiler.util.failureOf
import com.github.derg.transpiler.util.toFailure
import com.github.derg.transpiler.util.toSuccess
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

class TestPatterns
{
    @Nested
    inner class OneOfPattern
    {
        private val pattern = PatternOneOf(ParserBool, ParserReal)
        
        @Test
        fun `Given valid values, when parsing, then correctly parsed`()
        {
            assertEquals(true.value.toSuccess(), pattern.parse("true"))
            assertEquals(0.value.toSuccess(), pattern.parse("0"))
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct error`()
        {
            val expected = "no matching pattern: ['if' is not a bool, 'if' is not a number]"
            
            assertEquals(expected.toFailure(), pattern.parse("if"))
        }
        
        @Test
        fun `Given insufficient tokens, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), pattern.parse(""))
        }
    }
    
    @Nested
    inner class AnyOfPattern
    {
        private val pattern = PatternAnyOf(ParserBool, ParserReal)
        
        @Test
        fun `Given valid values, when parsing, then correctly parsed`()
        {
            assertEquals(true.value.toSuccess(), pattern.parse("true"))
            assertEquals(0.value.toSuccess(), pattern.parse("0"))
        }
        
        @Test
        fun `Given valid values, when parsing, then correct index`()
        {
            val context = contextOf("0 1")
            
            assertEquals(0.value.toSuccess(), pattern.parse(context))
            assertEquals(1, context.snapshot().index)
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct error`()
        {
            val expected = "no matching pattern: ['if' is not a bool, 'if' is not a number]"
            
            assertEquals(expected.toFailure(), pattern.parse("if"))
        }
        
        @Test
        fun `Given insufficient tokens, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), pattern.parse(""))
        }
    }
    
    @Nested
    inner class AllOfPattern
    {
        private val pattern = PatternAllOf("bool" to ParserBool, "real" to ParserReal)
        
        @Test
        fun `Given valid values, when parsing, then correctly parsed`()
        {
            val expected = mapOf("bool" to true.value, "real" to 0.value)
            
            assertEquals(expected.toSuccess(), pattern.parse("true 0"))
            assertEquals(expected.toSuccess(), pattern.parse("0 true"))
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct error`()
        {
            val expected = "no matching pattern: ['if' is not a bool, 'if' is not a number]"
            
            assertEquals(expected.toFailure(), pattern.parse("if"))
        }
        
        @Test
        fun `Given insufficient tokens, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), pattern.parse(""))
        }
    }
    
    @Nested
    inner class SequencePattern
    {
        private val pattern = PatternSequence(ParserBool, ParserReal)
        
        @Test
        fun `Given valid values, when parsing, then correctly parsed`()
        {
            val expected = listOf(true.value, 0.value)
            
            assertEquals(expected.toSuccess(), pattern.parse("true 0"))
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct error`()
        {
            assertEquals(failureOf("'if' is not a bool"), pattern.parse("if"))
        }
        
        @Test
        fun `Given insufficient tokens, when parsing, then correct error`()
        {
            assertEquals(failureOf("expected token, found end of stream"), pattern.parse(""))
        }
    }
    
    @Nested
    inner class OptionalPattern
    {
        private val pattern = PatternOptional(ParserBool, false.value)
        
        @Test
        fun `Given valid values, when parsing, then correctly parsed`()
        {
            assertEquals(true.value.toSuccess(), pattern.parse("true"))
        }
        
        @Test
        fun `Given valid values, when parsing, then correct index`()
        {
            val context = contextOf("true")
            
            assertEquals(true.value.toSuccess(), pattern.parse(context))
            assertEquals(1, context.snapshot().index)
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct index`()
        {
            val context = contextOf("0")
            
            assertEquals(false.value.toSuccess(), pattern.parse(context))
            assertEquals(0, context.snapshot().index)
        }
        
        @Test
        fun `Given invalid values, when parsing, then correct error`()
        {
            assertEquals(false.value.toSuccess(), pattern.parse(""))
            assertEquals(false.value.toSuccess(), pattern.parse("if"))
        }
    }
}
