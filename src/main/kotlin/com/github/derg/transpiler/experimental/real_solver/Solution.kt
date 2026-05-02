package com.github.derg.transpiler.experimental.real_solver

import com.github.derg.transpiler.source.thir.*
import java.util.*

/**
 * The solution stores the current state of all type information in the program. The solution can be nested inside a
 * [parent], where the solution inherits all values in the parent solution. This is necessary to handle branching and
 * resolution of overloads, where a type cannot be committed until we are sure the chosen overload is the only viable
 * candidate.
 */
class Solution(private val parent: Solution?)
{
    private val types = mutableMapOf<UUID, ThirType>()
    
//    /**
//     * Generates a new type variable representing the expression type. Note that the type variable must be constrained
//     * in order to be solved by the solver.
//     */
//    fun generateFlexible(): ThirType.Var =
//        ThirType.Var(UUID.randomUUID()).also { types[it.nodeId] = it }
    
    /**
     * Determines what the type of the element with the given [id] is. If the current solution does not contain the
     * element, we attempt to look up the type in the parent, recursively.
     */
    operator fun get(id: UUID): ThirType? = types[id] ?: parent?.get(id)
    
    /**
     * Marks the element with the given [id] as resolved with the given [type]. Note that the effects of this
     * substitution may not take effect until the solution is merged to the parent, if a parent is provided. See the
     * [commit] method for further details.
     */
    operator fun set(id: UUID, type: ThirType) = types.set(id, type)
    
    /**
     * Commits all types in this solution to the parent solution, if any. If the solution does not have any parent, this
     * method does nothing. Note that commitment is not recursive; only the outermost solution is merged, any deeper
     * nesting must be manually committed.
     */
    fun commit()
    {
        if (parent != null)
            parent.types += types
    }
}
