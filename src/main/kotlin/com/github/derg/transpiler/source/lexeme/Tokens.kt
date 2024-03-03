package com.github.derg.transpiler.source.lexeme

import com.github.derg.transpiler.source.*
import java.math.*

/**
 * A single token represents a single lexeme in the source code. Tokens may be extracted from the source code in various
 * manners depending on each type of lexeme, although the overall result may be summarized as a single token type.
 */
sealed interface Token

/**
 * The very last token in the sequence of tokens; it marks the end of the token stream.
 */
data object EndOfFile : Token

/**
 * All symbols such as variables, functions, type, namespaces, packages, etc. must be given a unique name. The
 * identifier token represents any such object by [name], although the token does not hold information determining which
 * object type it is applicable to.
 */
data class Identifier(val name: String) : Token

/**
 * The token holds a raw numerical literal of a specific [value] and optional [type].
 */
data class Numeric(val value: BigInteger, val type: String?) : Token

/**
 * The token holds a raw textual literal of a specific [value] and optional [type].
 */
data class Textual(val value: String, val type: String?) : Token

/**
 * Source code requires the use of keywords, structural symbols, as well as operators to specify what various objects
 * are, how the logic of the program flows, and which operations are to be performed. The token represents the [type] of
 * symbol.
 */
data class Keyword(val type: Symbol) : Token
