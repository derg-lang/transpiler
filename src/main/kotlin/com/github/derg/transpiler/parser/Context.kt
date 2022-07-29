package com.github.derg.transpiler.parser

import com.github.derg.transpiler.lexer.Token

/**
 * Holds all relevant information regarding the parsing of the source code. The information is used to determine how the
 * abstract syntax tree should be constructed from the source code.
 */
class Context(private val tokens: List<Token>)
{
    private var current = 0
    private var previous = 0
    
    /**
     * Determines whether there are more tokens which may be consumed from this context or not.
     */
    fun hasNext(): Boolean = current < tokens.size
    
    /**
     * Retrieves the next token in the sequence, if there are any remaining tokens. Invoking this method will progress
     * the context to the next token while returning the token at the current location. If no more tokens exists, no
     * further progress is made.
     */
    fun next(): Token? = tokens.getOrNull(current++)
    
    /**
     * The current committed context cursor position. This holds the index of the token the context is currently
     * targeting. By applying a pattern which successfully extracts meaningful data, the context cursor position is
     * moved forwards.
     */
    fun snapshot(): Int = current
    
    /**
     * Reverts the context cursor to the specified [snapshot]. This effectively moves the current parsing location back
     * to a previous location; this will typically occur when the parsing finds a dead-end or a token which does not
     * match the pattern currently being parsed.
     */
    fun revert(snapshot: Int)
    {
        current = snapshot
        previous = snapshot
    }
    
    /**
     * Resets the context cursor to the previous position which was committed. This ensures the context will always be
     * reverted to the current position when any parsing has been rolled back due to unexpected failures.
     */
    fun reset()
    {
        current = previous
    }
    
    /**
     * Moves the context cursor forwards one step. This is required to consume the token which has been examined, to
     * prevent it from being excessively consumed even though it cannot be. By committing a token, the token cannot be
     * consumed again at a later time (unless a rollback is applied).
     */
    fun commit()
    {
        previous = current
    }
}
