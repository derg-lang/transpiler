package com.github.derg.transpiler.experimental.resolver

import com.github.derg.transpiler.source.thir.*

/**
 * The set of constraints which are relevant to the type-checker. Constraints narrow down what a type may be, and are
 * used to solve the full type solution of a program.
 */
sealed interface Constraint
{
    /**
     * Indicates that the [lhs] and [rhs] kind are the exact same kind. If one of the kinds is `type`, both must resolve
     * to a type. If the kind is `value`, then the types of both values must be identical.
     */
    data class KindEq(val lhs: ThirKind, val rhs: ThirKind) : Constraint
    
    /**
     * Exactly one of the [branches] must succeed for type inference to pass. The collection of branches are used to
     * represent the possible overloads which are available when invoking a function, or looking up an identifier by
     * name.
     */
    data class OneOf(val branches: List<List<Constraint>>) : Constraint
}
