package com.github.derg.transpiler.parser

import com.github.derg.transpiler.lexer.Token

/**
 * The sequence of input [tokens] to parse and the current parse position is tracked in the parsing context. The context
 * keeps track of where in the process of parsing the parser is.
 */
data class Context(private val tokens: List<Token>)
{
    /**
     * The current position within the token stream. The position may be used to repeat over the same position within
     * the token stream multiple times, depending on the pattern being parsed.
     */
    private var cursor = 0
    
    /**
     * Retrieves a snapshot of the current state of the context.
     */
    fun snapshot(): Snapshot = Snapshot(cursor)
    
    /**
     * Restores the context to the specified [snapshot]. This enables the processing of tokens to resume a previous
     * processing path, where the lookahead parsing failed in one branch.
     */
    fun restore(snapshot: Snapshot)
    {
        cursor = snapshot.index
    }
    
    /**
     * Retrieves the element at the cursor location, and increments the current cursor position by one. If no more
     * tokens are available, `null` is returned.
     */
    fun next(): Token? = tokens.getOrNull(cursor++)
    
    /**
     * Determines whether there are additional tokens remaining to be consumed in the context.
     */
    fun hasNext(): Boolean = cursor < tokens.size
}

/**
 * The [Context] describes the current parsing state of the input source code. During parsing, a parsing branch may
 * result in no matching patterns, in which case a snapshot of the context is generated ahead of time. The snapshot
 * describes the context at an instant, enabling rollbacks to occur if the branch did not match any known patterns.
 */
data class Snapshot(val index: Int)
