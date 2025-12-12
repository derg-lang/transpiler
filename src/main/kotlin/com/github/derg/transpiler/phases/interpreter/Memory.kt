package com.github.derg.transpiler.phases.interpreter

import com.github.derg.transpiler.source.thir.*
import java.util.*

/**
 * A stack frame contains all variables which are defined within a specific function call. Frames are used to capture
 * the state of the program before, during, and after a function call is complete. Each frame contains the values which
 * are pushed onto the stack, and will no longer be available once the stack is popped.
 */
class StackFrame
{
    private val memory = mutableMapOf<UUID, ThirExpression.Canonical>()
    
    /**
     * Determines whether the given [id] is defined in this frame or not.
     */
    operator fun contains(id: UUID): Boolean
    {
        return memory[id] != null
    }
    
    /**
     * Retrieves the value associated with the given [id]. This method should only be invoked on symbols which
     * definitely have been registered previously.
     */
    operator fun get(id: UUID): ThirExpression.Canonical
    {
        return memory[id] ?: throw IllegalStateException("Attempted to access value '$id', but it is undefined")
    }
    
    /**
     * Retrieves the value associated with the given [id]. This method should only be invoked on symbols which
     * definitely have been registered previously.
     */
    operator fun set(id: UUID, value: ThirExpression.Canonical)
    {
        memory[id] = value
    }
}
