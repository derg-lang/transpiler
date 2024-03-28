package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * The instruction checker ensures that all instructions within some callable context are valid. The instructions are expected to output the [value] and [error]
 * types, if they are specified.
 */
internal class CheckerInstruction(private val value: ThirType?, private val error: ThirType?)
{
    /**
     * Performs type-checking on the instruction [node]. If there are any type errors detected, an error is returned.
     */
    fun check(node: ThirInstruction): Result<Unit, TypeError> = when (node)
    {
        is ThirAssign      -> TODO()
        is ThirBranch      -> handle(node)
        is ThirEvaluate    -> handle(node)
        is ThirReturn      -> handle(node)
        is ThirReturnError -> handle(node)
        is ThirReturnValue -> handle(node)
    }
    
    private fun handle(node: ThirBranch): Result<Unit, TypeError>
    {
        // Predicates are not permitted to contain error types.
        if (node.predicate.error != null)
            return TypeError.BranchHasError(node.predicate).toFailure()
        
        // The value type must be boolean.
        val value = node.predicate.value as? ThirTypeData
        if (value == null || value.symbolId != Builtin.BOOL.id)
            return TypeError.BranchWrongValue(node.predicate).toFailure()
        
        // TODO: Type-check the predicate too, ensure that it does not contain any forbidden values either.
        return Unit.toSuccess()
    }
    
    private fun handle(node: ThirEvaluate): Result<Unit, TypeError>
    {
        // Evaluations are not permitted to contain error types.
        if (node.expression.error != null)
            return TypeError.EvaluateHasError(node.expression).toFailure()
        
        // Evaluations are not permitted to contain value types.
        if (node.expression.value != null)
            return TypeError.EvaluateHasValue(node.expression).toFailure()
        
        // TODO: Type-check the expression too, ensure that it does not contain any forbidden values either.
        return Unit.toSuccess()
    }
    
    private fun handle(node: ThirReturn): Result<Unit, TypeError>
    {
        // If the context requires an error to be raised, we cannot simply bail from the callable.
        if (error != null)
            return TypeError.ReturnLacksError.toFailure()
        
        // If the context requires a value to be returned, we cannot simply bail from the callable.
        if (value != null)
            return TypeError.ReturnLacksValue.toFailure()
        
        return Unit.toSuccess()
    }
    
    private fun handle(node: ThirReturnValue): Result<Unit, TypeError>
    {
        // If the function is not permitted to return anything, we cannot return anything.
        if (value == null)
            return TypeError.ReturnHasValue(node.expression).toFailure()
        
        // If the expected value type is different, we have incompatible types.
        // TODO: Support generics.
        // TODO: Support mutable types.
        // TODO: Support union types.
        if (value != node.expression.value)
            return TypeError.ReturnWrongValue(node.expression).toFailure()
        
        // TODO: Type-check the expression too, ensure that it does not contain any forbidden values either.
        return Unit.toSuccess()
    }
    
    private fun handle(node: ThirReturnError): Result<Unit, TypeError>
    {
        // If the function is not permitted to raise anything, we cannot raise anything.
        if (error == null)
            return TypeError.ReturnHasError(node.expression).toFailure()
        
        // If the expected error type is different, we have incompatible types.
        // TODO: Support generics.
        // TODO: Support mutable types.
        // TODO: Support union types.
        if (error != node.expression.error)
            return TypeError.ReturnWrongError(node.expression).toFailure()
        
        // TODO: Type-check the expression too, ensure that it does not contain any forbidden values either.
        return Unit.toSuccess()
    }
}
