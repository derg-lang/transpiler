package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.source.thir.*

/**
 * During the type checking phase, a variety of type errors may arise. These errors describe why a type cannot be used
 * in the place the developer has attempted.
 */
sealed interface TypeError
{
    /**
     * The branch [predicate] did not evaluate to a boolean type, which is required by the branch predicate.
     */
    data class BranchWrongValue(val predicate: ThirValue) : TypeError
    
    /**
     * The branch [predicate] is evaluated to a possible error type, which is not permitted.
     */
    data class BranchContainsError(val predicate: ThirValue) : TypeError
    
    /**
     * The provided [expression] is evaluated to a possible value, which is not permitted.
     */
    data class EvaluateContainsValue(val expression: ThirValue) : TypeError
    
    /**
     * The provided [expression] is evaluated to a possible error, which is not permitted.
     */
    data class EvaluateContainsError(val expression: ThirValue) : TypeError
    
    /**
     * The return statement lacks a value associated with it, when the callable expected something to be returned.
     */
    data object ReturnMissingExpression : TypeError
    
    /**
     * The return statement has an [expression] which evaluates to no type at all.
     */
    data class ReturnMissingValue(val expression: ThirValue) : TypeError
    
    /**
     * The return statement has an [expression] which evaluates to the wrong type.
     */
    data class ReturnWrongType(val expression: ThirValue) : TypeError
    
    /**
     * The return statement has an [expression] which evaluates to a value, when no return value was expected.
     */
    data class ReturnContainsValue(val expression: ThirValue) : TypeError
    
    /**
     * The return statement has an [expression] which evaluates to an error, when no return error was expected.
     */
    data class ReturnContainsError(val expression: ThirValue) : TypeError
}
