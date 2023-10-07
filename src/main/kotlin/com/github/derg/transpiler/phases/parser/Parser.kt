package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.phases.lexer.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.utils.*

/**
 * Parses the [input] string into a single segment, forming a part of the overall program. The total collection of all
 * segments makes up the program.
 */
fun parse(input: String): Result<AstSegment, ParseError>
{
    val parser = segmentParserOf()
    val tokens = tokenize(input).map { it.data } + EndOfFile
    tokens.fold { parser.parse(it) }.valueOr { return it.toFailure() }
    return parser.produce().toSuccess()
}

/**
 * All data which is read from the source code which is parsed must be converted into individual nodes. All nodes
 * together form the abstract syntax tree describing the full source code. The abstract syntax tree may be utilized to
 * perform various operations on the source code in a manner which is simpler than performing text manipulations.
 */
interface Parser<out Type>
{
    /**
     * Determines whether the parser requires at least one token in order to produce any meaningful content or not. Any
     * parser which is marked as skipable may not be provided any tokens at all.
     */
    fun skipable(): Boolean
    
    /**
     * Pulls out the produced value from the parser, if it contains any finished items. A parser can only contain any
     * finished items by being provided with enough tokens to construct the item.
     */
    fun produce(): Type
    
    /**
     * Provides a single new [token] to the parser. If the parser accepts the token, a success is returned. If the
     * parser does not accept it, a failure is returned, containing the details for why the token was rejected. Parsers
     * should be provided new tokens until it explicitly states that it will accept no more tokens.
     */
    fun parse(token: Token): Result<ParseOk, ParseError>
    
    /**
     * Reverts all state in the parser back to the default settings. In essence, this wipes all data related to the
     * parser completely clean, meaning it holds no information about previous parses.
     */
    fun reset()
}

/**
 * Parsers require tokens to perform their workload. While they are able to accept tokens, their internal state is
 * revealed by the parse outcome. Some parsers will require an infinite stream of additional tokens, whereas some tokens
 * are satisfied by a finite number of tokens.
 */
sealed interface ParseOk
{
    /**
     * The parser is currently in process of assembling additional data, and thus will require more tokens.
     */
    data object Incomplete : ParseOk
    
    /**
     * The parser has received enough tokens to complete assembling its data. It may potentially require more tokens to
     * further assemble additional data, but is currently in an indeterminate state - another token must be provided to
     * determine the final state.
     */
    data object Complete : ParseOk
    
    /**
     * The parser has received enough tokens to finish its data, and will not need additional tokens. The parser has
     * rejected the provided token. The token should be provided to the next candidate parser instead.
     */
    data object Finished : ParseOk
}

/**
 * Whenever source code is parsed into an abstract syntax tree, various error conditions prevent further processing from
 * taking place. This is always the case if the source code ends abruptly or otherwise does not respect the grammar of
 * the programming language.
 */
sealed interface ParseError
{
    /**
     * The parser expected a specific token, but instead received an unexpected [token].
     */
    data class UnexpectedToken(val token: Token) : ParseError
}
