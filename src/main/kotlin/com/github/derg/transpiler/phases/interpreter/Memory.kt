package com.github.derg.transpiler.phases.interpreter

import com.github.derg.transpiler.source.thir.*
import java.util.*

/**
 * TODO: Write me!
 */
class Stack
{
    private val memory = mutableListOf<ThirExpression.Canonical>()
    private val frames = mutableListOf(mutableMapOf<UUID, ThirExpression.Address>())
    
    /**
     * Register a new constant value into the stack under the given [symbolId]. The [value] is considered a constant and
     * should never be updated. This method should only be invoked during compilation, when constants are registered in
     * the global scope.
     */
    fun register(symbolId: UUID, value: ThirExpression.Canonical): ThirExpression.Address
    {
        val address = ThirExpression.Address(memory.size)
        memory += value
        frames.last()[symbolId] = address
        return address
    }
    
    /**
     * Pushes a series of [values] onto the stack. The keys are the symbol ids which are associated with the values,
     * each value will be given its own address on the stack. When the [function] is done executing, the values will be
     * popped from the stack.
     */
    fun <Type> push(values: Map<UUID, ThirExpression.Canonical>, function: () -> Type): Type
    {
        val frame = mutableMapOf<UUID, ThirExpression.Address>()
        for ((symbolId, value) in values)
        {
            frame[symbolId] = ThirExpression.Address(memory.size)
            memory += value
        }
        
        frames.add(frame)
        val value = function()
        frames.removeLast()
        return value
    }
    
    /**
     * Retrieves the address of the given [symbolId], if it exists. All references are resolved during this process,
     * ensuring that the final address is the one referenced.
     */
    fun addressOf(symbolId: UUID): ThirExpression.Address?
    {
        // Find the value pointed to. We search the whole stack until we find the symbol where it is registered. If the
        // symbol could not be found, that is that - we cannot look up the address in that case.
        var address = frames.asReversed().firstNotNullOfOrNull { it[symbolId] } ?: return null
        
        // Otherwise, we trace down the address until we have followed all references all the way. All references
        // resolve to the final address where the value is located in the end.
        while (true)
        {
            val value = memory[address.raw]
            if (value !is ThirExpression.Address)
                return address
            address = value
        }
    }
    
    /**
     * Retrieves the value stored at the given [address] in the stack. A value must have been pushed to that address
     * before it can be accessed.
     */
    operator fun get(address: ThirExpression.Address): ThirExpression.Canonical
    {
        return memory[address.raw]
    }
    
    /**
     * Assigns the [value] to the given [address] in the stack. A value must have been pushed to that address before it
     * can be modified.
     */
    operator fun set(address: ThirExpression.Address, value: ThirExpression.Canonical)
    {
        memory[address.raw] = value
    }
}
