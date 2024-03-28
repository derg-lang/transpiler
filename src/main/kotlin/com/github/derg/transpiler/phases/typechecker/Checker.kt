package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.source.thir.*

/**
 * During the type checking phase, a variety of type errors may arise. These errors describe why a type cannot be used
 * in the place the developer has attempted.
 */
sealed interface TypeError
{
    /**
     * The provided [value] did not evaluate to a boolean type, which is required by the branch predicate.
     */
    data class BranchPredicateNotBool(val value: ThirValue) : TypeError
    
    /**
     * The branch predicate contains an [error] type which is not permitted. Predicates must always succeed.
     */
    data class BranchPredicateHasError(val error: ThirValue) : TypeError
    
    /**
     * The provided [value] was evaluated to have a value type, which is not permitted.
     */
    data class EvaluateHasValue(val value: ThirValue): TypeError
    
    /**
     * The provided [error] was evaluated to have a value type, which is not permitted.
     */
    data class EvaluateHasError(val error: ThirValue): TypeError
}
