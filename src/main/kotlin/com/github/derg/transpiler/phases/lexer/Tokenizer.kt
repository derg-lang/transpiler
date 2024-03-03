package com.github.derg.transpiler.phases.lexer

import com.github.derg.transpiler.core.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.utils.*

/**
 * Accepts a source code [input] string, and converts it into a sequence of tokens.
 */
fun tokenize(input: String): List<Localized<Token>> = TokenExtractor("", input).toList()

/**
 * The tokenizer accepts an [input], and allows the string to be processed iteratively until all tokens within the
 * string have been exhausted.
 */
private class TokenExtractor(private val file: String, private val input: String) :
    Iterator<Localized<Token>>, Iterable<Localized<Token>>
{
    private var cursor = 0
    
    override fun iterator(): Iterator<Localized<Token>> = this
    override fun hasNext(): Boolean = extractToken(input, cursor) != null
    override fun next(): Localized<Token>
    {
        val (token, span) = extractToken(input, cursor)
            ?: throw IllegalStateException("No token was found in '$input' at position $cursor")
        cursor = span.last
        return Localized(Location(file, span.first, span.last - span.first), token)
    }
}

/**
 * Defines the interface which must be respected by any tokenizer. Tokenizers are used to retrieve the next token in the
 * source code, although not all tokenizers are capable of understanding the source at the cursor position.
 */
private typealias Tokenizer = (String, Int) -> Pair<Token, IntRange>?

/**
 * The collection of all tokenizers which are capable of extracting a token from source code. Exactly one tokenizer must
 * be used to retrieve the next token from the source code.
 */
private val TOKENIZERS: List<Tokenizer> = listOf(
    ::extractSymbol,
    ::extractNumber,
    ::extractString,
    ::extractIdentifier,
)

/**
 * Extracts the token at the [cursor] position from the [input] string.
 */
private fun extractToken(input: String, cursor: Int): Pair<Token, IntRange>?
{
    if (cursor >= input.length)
        return null
    if (input[cursor].isWhitespace())
        return extractToken(input, cursor + 1)
    
    return TOKENIZERS.mapNotNull { it(input, cursor) }.maxByOrNull { it.second.last }
        ?: throw IllegalStateException("No tokenizers matched '$input' at position $cursor")
}

/**
 * Extracts the symbol which matches the [input] at the [cursor] location.
 */
private fun extractSymbol(input: String, cursor: Int): Pair<Token, IntRange>?
{
    val value = Symbol.values()
        .filter { it.symbol == input.substringFrom(cursor, cursor + it.symbol.length) }
        .maxByOrNull { it.symbol.length } ?: return null
    return Keyword(value) to IntRange(cursor, cursor + value.symbol.length)
}

/**
 * Extracts the identifier in [input] starting at [cursor].
 */
private fun extractIdentifier(input: String, cursor: Int): Pair<Token, IntRange>?
{
    // Identifiers cannot start with a number (`45s` is interpreted as the literal `45` of type `s`)
    if (isLegalInIdentifier(input[cursor]) && input[cursor] !in '0'..'9')
        return extractIdentifierWithoutBacktick(input, cursor)
    if (input[cursor] == '`')
        return extractIdentifierWithBacktick(input, cursor)
    return null
}

private fun extractIdentifierWithoutBacktick(input: String, cursor: Int): Pair<Token, IntRange>
{
    val endIndex = input.indexOfFirstOrNull(cursor) { !isLegalInIdentifier(it) } ?: input.length
    return Identifier(input.substringFrom(cursor, endIndex)) to IntRange(cursor, endIndex)
}

private fun extractIdentifierWithBacktick(input: String, cursor: Int): Pair<Token, IntRange>?
{
    val endIndex = input.indexOfOrNull('`', cursor + 1) ?: return null
    return Identifier(input.substring(cursor + 1, endIndex)) to IntRange(cursor, endIndex + 1)
}

/**
 * Extracts the string in [input] starting at [cursor].
 */
private fun extractString(input: String, cursor: Int): Pair<Token, IntRange>?
{
    // TODO: Add capability of escaping quotes, raw strings, line breaks, etc.
    if (input[cursor] != '"')
        return null
    val typeIndex = input.indexOfOrNull('"', cursor + 1) ?: return null
    val endIndex = input.indexOfFirstOrNull(typeIndex + 1) { !isLegalInIdentifier(it) } ?: input.length
    
    val value = input.substring(cursor + 1, typeIndex)
    val type = input.substringFrom(typeIndex + 1, endIndex).ifBlank { null }
    return Textual(value, type) to IntRange(cursor, endIndex)
}

/**
 * Extracts the number in [input] starting at [cursor].
 */
private fun extractNumber(input: String, cursor: Int): Pair<Token, IntRange>?
{
    // TODO: Add capability of separating groups of numbers, hexadecimal, binary, octal numbers, etc.
    if (!isLegalInNumber(input[cursor]))
        return null
    val typeIndex = input.indexOfFirstOrNull(cursor) { !isLegalInNumber(it) } ?: input.length
    val endIndex = input.indexOfFirstOrNull(typeIndex) { !isLegalInIdentifier(it) } ?: input.length
    
    val value = input.substringFrom(cursor, typeIndex).toBigDecimalOrNull() ?: return null
    val type = input.substringFrom(typeIndex, endIndex).ifBlank { null }
    return Numeric(value, type) to IntRange(cursor, endIndex)
}

/**
 * Helper function for determining whether the given [character] may appear in an identifier or not.
 */
fun isLegalInIdentifier(character: Char): Boolean =
    character == '_' || character in 'a'..'z' || character in 'A'..'Z' || character in '0'..'9'

/**
 * Helper function for determining whether the given [character] may appear in a number or not.
 */
fun isLegalInNumber(character: Char): Boolean =
    character == '.' || character in '0'..'9'
