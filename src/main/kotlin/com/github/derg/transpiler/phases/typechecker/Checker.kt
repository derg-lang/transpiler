package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * Performs type-checking across all entries in the symbol [table].
 */
// TODO: Super-superficial type-checker operating on a whole symbol table.
fun check(table: SymbolTable): Result<Unit, TypeError>
{
    for (function in table.functions.values)
    {
        val checker = when (function.type)
        {
            is ThirTypeFunction -> CheckerInstruction(function.type.value, function.type.error)
            is ThirTypeLiteral  -> CheckerInstruction(function.type.value, null)
        }
        
        function.instructions.mapUntilError { checker.check(it) }.valueOr { return it.toFailure() }
    }
    
    return Unit.toSuccess()
}

/**
 * During the type checking phase, a variety of type errors may arise. These errors describe why a type cannot be used
 * in the place the developer has attempted.
 */
sealed interface TypeError
{
    /**
     * The assignment [expression] evaluates to no type at all.
     */
    data class AssignMissingValue(val expression: ThirValue) : TypeError
    
    /**
     * The assignment [expression] did not evaluate to a type compatible with the variable.
     */
    data class AssignWrongType(val expression: ThirValue) : TypeError
    
    /**
     * The assignment [expression] is evaluated to a possible error type, which is not permitted.
     */
    data class AssignContainsError(val expression: ThirValue) : TypeError
    
    /**
     * The branch [predicate] evaluates to no type at all.
     */
    data class BranchMissingValue(val predicate: ThirValue) : TypeError
    
    /**
     * The branch [predicate] did not evaluate to a boolean type, which is required by the branch predicate.
     */
    data class BranchWrongType(val predicate: ThirValue) : TypeError
    
    /**
     * The branch [predicate] is evaluated to a possible error type, which is not permitted.
     */
    data class BranchContainsError(val predicate: ThirValue) : TypeError
    
    /**
     * The call [instance] does not evaluate to something which has any value type.
     */
    data class CallMissingValue(val instance: ThirValue) : TypeError
    
    /**
     * The call [instance] did not evaluate to a type which is callable.
     */
    data class CallWrongType(val instance: ThirValue) : TypeError
    
    /**
     * The call [instance] is evaluated to a possible error type, which is not permitted.
     */
    data class CallContainsError(val instance: ThirValue) : TypeError
    
    /**
     * The catch [instance] does not evaluate to something which has any value type.
     */
    data class CatchMissingValue(val instance: ThirValue) : TypeError
    
    /**
     * The catch [instance] does not evaluate to something which has any error type.
     */
    data class CatchMissingError(val instance: ThirValue) : TypeError
    
    /**
     * The catch [instance] is evaluated to a possible error type, which is not permitted.
     */
    data class CatchContainsError(val instance: ThirValue) : TypeError
    
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
