package com.github.derg.transpiler.experimental.resolver

import java.util.*

/**
 * The set of constraints which are relevant to the type-checker. Constraints narrow down what a type may be, and are
 * used to solve the full type solution of a program.
 */
sealed interface Constraint
{
    /**
     * Marks the given [nodeId] as associated with the given [correlationId]. The solution id is a value stored and
     * persisted for disambiguation during identifier resolution. When multiple symbols are competing with each other
     * during the type-checking phase, a tag may be used to mark determine which [OneOf] branch succeeded. Each branch
     * should include exactly one tag per node within that branch.
     */
    data class Tag(
        val nodeId: UUID,
        val correlationId: UUID,
    ) : Constraint
    
    /**
     * Indicates that the [lhs] and [rhs] kind are the exact same kind. If one of the kinds is `type`, both must resolve
     * to a type. If the kind is `value`, then the types of both values must be identical.
     */
    data class TypeEq(
        val lhs: ThirType,
        val rhs: ThirType,
    ) : Constraint
    
    /**
     * Exactly one of the [branches] must succeed for type inference to pass. The collection of branches are used to
     * represent the possible overloads which are available when invoking a function, or looking up an identifier by
     * name.
     */
    data class OneOf(
        val branches: List<List<Constraint>>,
    ) : Constraint
}
