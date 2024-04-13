package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * The instruction checker ensures that all instructions within some contexts are valid. The instructions are expected
 * to output the [value] and [error] types, if they are specified. For some instructions, the types are not applicable.
 */
internal class CheckerInstruction(private val value: ThirType?, private val error: ThirType?)
{
    /**
     * Performs type-checking on the instruction [node]. If there are any type errors detected, an error is returned.
     */
    fun check(node: ThirInstruction): Result<Unit, TypeError> = when (node)
    {
        is ThirAssign      -> handle(node)
        is ThirBranch      -> handle(node)
        is ThirEvaluate    -> handle(node)
        is ThirReturn      -> handle(node)
        is ThirReturnError -> handle(node)
        is ThirReturnValue -> handle(node)
    }
    
    private fun handle(node: ThirAssign): Result<Unit, TypeError>
    {
        // The expression must evaluate to exactly a value type, no error type is permitted.
        if (node.expression.value == null)
            return TypeError.AssignMissingValue(node.expression).toFailure()
        if (node.expression.error != null)
            return TypeError.AssignContainsError(node.expression).toFailure()
        
        // If the expected type is different, we have incompatible types.
        // TODO: Support generics.
        // TODO: Support mutable types.
        // TODO: Support union types.
        if (value != node.expression.value)
            return TypeError.AssignWrongType(node.expression).toFailure()
    
        return CheckerValue().check(node.expression)
    }
    
    private fun handle(node: ThirBranch): Result<Unit, TypeError>
    {
        // Predicates are not permitted to contain error types.
        if (node.predicate.error != null)
            return TypeError.BranchContainsError(node.predicate).toFailure()
        
        // The value type must be boolean.
        val value = node.predicate.value ?: return TypeError.BranchMissingValue(node.predicate).toFailure()
        if (value !is ThirTypeData || value.symbolId != Builtin.BOOL.id)
            return TypeError.BranchWrongType(node.predicate).toFailure()
    
        return CheckerValue().check(node.predicate)
    }
    
    private fun handle(node: ThirEvaluate): Result<Unit, TypeError>
    {
        // Evaluations are not permitted to contain error types.
        if (node.expression.error != null)
            return TypeError.EvaluateContainsError(node.expression).toFailure()
        
        // Evaluations are not permitted to contain value types.
        if (node.expression.value != null)
            return TypeError.EvaluateContainsValue(node.expression).toFailure()
    
        return CheckerValue().check(node.expression)
    }
    
    private fun handle(node: ThirReturn): Result<Unit, TypeError>
    {
        // If the context requires a value to be returned, we cannot simply bail from the callable.
        if (value != null)
            return TypeError.ReturnMissingExpression.toFailure()
        
        return Unit.toSuccess()
    }
    
    private fun handle(node: ThirReturnValue): Result<Unit, TypeError>
    {
        // If the function is not permitted to return anything, we cannot return anything.
        if (value == null)
            return TypeError.ReturnContainsValue(node.expression).toFailure()
        
        // The expression must evaluate to exactly a value type, no error type is permitted.
        if (node.expression.value == null)
            return TypeError.ReturnMissingValue(node.expression).toFailure()
        if (node.expression.error != null)
            return TypeError.ReturnContainsError(node.expression).toFailure()
        
        // If the expected type is different, we have incompatible types.
        // TODO: Support generics.
        // TODO: Support mutable types.
        // TODO: Support union types.
        if (value != node.expression.value)
            return TypeError.ReturnWrongType(node.expression).toFailure()
        
        // TODO: Type-check the expression too, ensure that it does not contain any forbidden values either.
        return Unit.toSuccess()
    }
    
    private fun handle(node: ThirReturnError): Result<Unit, TypeError>
    {
        // If the function is not permitted to raise anything, we cannot raise anything.
        if (error == null)
            return TypeError.ReturnContainsValue(node.expression).toFailure()
        
        // The expression must evaluate to exactly a value type, no error type is permitted.
        if (node.expression.value == null)
            return TypeError.ReturnMissingValue(node.expression).toFailure()
        if (node.expression.error != null)
            return TypeError.ReturnContainsError(node.expression).toFailure()
        
        // If the expected type is different, we have incompatible types.
        // TODO: Support generics.
        // TODO: Support mutable types.
        // TODO: Support union types.
        if (error != node.expression.value)
            return TypeError.ReturnWrongType(node.expression).toFailure()
        
        // TODO: Type-check the expression too, ensure that it does not contain any forbidden values either.
        return Unit.toSuccess()
    }
}
