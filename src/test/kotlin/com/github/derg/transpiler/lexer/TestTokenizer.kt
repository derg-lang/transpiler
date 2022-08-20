package com.github.derg.transpiler.lexer

import com.github.derg.transpiler.core.Localized
import com.github.derg.transpiler.core.Location
import com.github.derg.transpiler.lexer.SymbolType.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

/**
 * Generates a new localized token.
 */
private fun localizedOf(cursor: Int, length: Int, token: Token): Localized<Token> =
    Localized(Location("", cursor, length), token)

/**
 * Generates a new localized token and wraps it inside a list.
 */
private fun localisedOfAsList(cursor: Int, length: Int, token: Token): List<Localized<Token>> =
    listOf(localizedOf(cursor, length, token))

class TestTokenizer
{
    /**
     * Ensures individual tokens are processed correctly by the tokenizer. All tokens must be extracted properly when
     * they are present in the code on their own at the bare minimum.
     */
    @Nested
    inner class SingleToken
    {
        @Test
        fun `Given blank string, when tokenizing, then no tokens`()
        {
            assertEquals(emptyList<Token>(), tokenize(""))
            assertEquals(emptyList<Token>(), tokenize(" "))
            assertEquals(emptyList<Token>(), tokenize("\t"))
            assertEquals(emptyList<Token>(), tokenize("\n"))
        }
        
        @Test
        fun `Given symbol, when tokenizing, then correct keyword`()
        {
            for (type in SymbolType.values())
                assertEquals(localisedOfAsList(0, type.symbol.length, Symbol(type)), tokenize(type.symbol))
        }
        
        @Test
        fun `Given numeric literal, when tokenizing, then correct number`()
        {
            assertEquals(localisedOfAsList(0, 1, Numeric(0.toBigDecimal(), null)), tokenize("0"))
            assertEquals(localisedOfAsList(0, 1, Numeric(1.toBigDecimal(), null)), tokenize("1"))
            assertEquals(localisedOfAsList(0, 4, Numeric(3.14.toBigDecimal(), null)), tokenize("3.14"))
            assertEquals(localisedOfAsList(0, 2, Numeric(0.5.toBigDecimal(), null)), tokenize(".5"))
            assertEquals(localisedOfAsList(0, 2, Numeric(0.toBigDecimal(), "s")), tokenize("0s"))
        }
        
        @Test
        fun `Given textual literal, when tokenizing, then correct string`()
        {
            assertEquals(localisedOfAsList(0, 2, Textual("", null)), tokenize("\"\""))
            assertEquals(localisedOfAsList(0, 5, Textual("foo", null)), tokenize("\"foo\""))
            assertEquals(localisedOfAsList(0, 14, Textual("Hello World!", null)), tokenize("\"Hello World!\""))
            assertEquals(localisedOfAsList(0, 8, Textual("foo", "bar")), tokenize("\"foo\"bar"))
        }
        
        @Test
        fun `Given identifier, when tokenizing, then correct identifier`()
        {
            // Arbitrary sequences of text containing legal characters
            assertEquals(localisedOfAsList(0, 1, Identifier("f")), tokenize("f"))
            assertEquals(localisedOfAsList(0, 8, Identifier("anything")), tokenize("anything"))
            assertEquals(localisedOfAsList(0, 13, Identifier("withMixedCase")), tokenize("withMixedCase"))
            assertEquals(localisedOfAsList(0, 15, Identifier("with_underscore")), tokenize("with_underscore"))
            assertEquals(localisedOfAsList(0, 13, Identifier("with_numb3er5")), tokenize("with_numb3er5"))
            
            // Starting or containing keywords is fine
            assertEquals(localisedOfAsList(0, 10, Identifier("if_keyword")), tokenize("if_keyword"))
            assertEquals(localisedOfAsList(0, 14, Identifier("something_else")), tokenize("something_else"))
            
            // Must also support backticks
            assertEquals(localisedOfAsList(0, 3, Identifier("f")), tokenize("`f`"))
            assertEquals(localisedOfAsList(0, 11, Identifier("name here")), tokenize("`name here`"))
            assertEquals(localisedOfAsList(0, 4, Identifier("if")), tokenize("`if`"))
        }
    }
    
    /**
     * Some combinations of tokens appear frequently and may have adverse effects on the tokenization process. By adding
     * a dedicated test suite, such issues may be explicitly handled in a more maintainable manner, rather than relying
     * on tests at a higher abstraction layer to catch them.
     */
    @Nested
    inner class DuoTokens
    {
        @Test
        fun `Given simple literal expression, when tokenizing, then correct sequence`()
        {
            val expected = listOf(
                localizedOf(0, 1, Numeric(0.toBigDecimal(), null)),
                localizedOf(1, 1, Symbol(SEMICOLON)),
            )
            
            assertEquals(expected, tokenize("0;"))
        }
        
        @Test
        fun `Given simple unary expression, when tokenizing, then correct sequence`()
        {
            val expected = listOf(
                localizedOf(0, 1, Symbol(MINUS)),
                localizedOf(1, 1, Numeric(5.toBigDecimal(), null)),
                localizedOf(2, 1, Symbol(SEMICOLON)),
            )
            
            assertEquals(expected, tokenize("-5;"))
        }
    }
    
    /**
     * Collections of tokens must be extractable by the tokenizer. The tokens must also be located in the proper
     * location in the source code, and must not conflict with each other in destructive manners.
     */
    @Nested
    inner class ComplexTokens
    {
        /**
         * Functions represents one of the most fundamental units which must be tokenized properly. The syntax of functions
         * are non-trivial and all cases for functions must be properly tokenized.
         */
        @Nested
        inner class Functions
        {
            @Test
            fun `Given function declaration, when tokenizing, then correct sequence`()
            {
                val expected = listOf(
                    localizedOf(0, 3, Symbol(FUN)),
                    localizedOf(4, 8, Identifier("function")),
                    localizedOf(12, 1, Symbol(SEMICOLON)),
                )
                
                assertEquals(expected, tokenize("fun function;"))
            }
            
            @Test
            fun `Given function declaration with return type, when tokenizing, then correct sequence`()
            {
                val expected = listOf(
                    localizedOf(0, 3, Symbol(FUN)),
                    localizedOf(4, 8, Identifier("function")),
                    localizedOf(13, 2, Symbol(ARROW)),
                    localizedOf(16, 5, Identifier("thing")),
                    localizedOf(21, 1, Symbol(SEMICOLON)),
                )
                
                assertEquals(expected, tokenize("fun function -> thing;"))
            }
            
            @Test
            fun `Given function declaration with parameter list, when tokenizing, then correct sequence`()
            {
                val expected = listOf(
                    localizedOf(0, 3, Symbol(FUN)),
                    localizedOf(4, 8, Identifier("function")),
                    localizedOf(12, 1, Symbol(COLON)),
                    localizedOf(14, 3, Identifier("foo")),
                    localizedOf(18, 6, Identifier("param1")),
                    localizedOf(24, 1, Symbol(COMMA)),
                    localizedOf(26, 3, Identifier("bar")),
                    localizedOf(30, 6, Identifier("param2")),
                    localizedOf(36, 1, Symbol(SEMICOLON)),
                )
                
                assertEquals(expected, tokenize("fun function: foo param1, bar param2;"))
            }
            
            @Test
            fun `Given function definition, when tokenizing, then correct sequence`()
            {
                val expected = listOf(
                    localizedOf(0, 3, Symbol(FUN)),
                    localizedOf(4, 8, Identifier("function")),
                    localizedOf(13, 1, Symbol(OPEN_BRACE)),
                    localizedOf(14, 1, Symbol(CLOSE_BRACE)),
                )
                
                assertEquals(expected, tokenize("fun function {}"))
            }
        }
        
        /**
         * Functions represents one of the most fundamental units which must be tokenized properly. The syntax of variables
         * are not too difficult, although all cases for variables must be properly tokenized.
         */
        @Nested
        inner class Variables
        {
            @Test
            fun `Given variable definition, when tokenizing, then correct sequence`()
            {
                val expected = listOf(
                    localizedOf(0, 3, Symbol(VAL)),
                    localizedOf(4, 8, Identifier("variable")),
                    localizedOf(13, 1, Symbol(ASSIGN)),
                    localizedOf(15, 2, Numeric(42.toBigDecimal(), null)),
                    localizedOf(17, 1, Symbol(SEMICOLON)),
                )
                
                assertEquals(expected, tokenize("val variable = 42;"))
            }
        }
        
        /**
         * Functions represents one of the most fundamental units which must be tokenized properly. The syntax of types is
         * quite extensive, and all cases for types must be properly tokenized.
         */
        @Nested
        inner class Types
        {
            @Test
            fun `Given type definition, when tokenizing, then correct sequence`()
            {
                val expected = listOf(
                    localizedOf(0, 4, Symbol(TYPE)),
                    localizedOf(5, 1, Identifier("t")),
                    localizedOf(6, 1, Symbol(SEMICOLON)),
                )
                
                assertEquals(expected, tokenize("type t;"))
            }
        }
    }
}
