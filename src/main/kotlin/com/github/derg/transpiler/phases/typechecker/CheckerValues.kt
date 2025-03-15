package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * The value checker ensures that the value examined is valid in the context it is used in.
 */
internal class CheckerValue
{
    /**
     * Performs type-checking on the instruction [node]. If there are any type errors detected, an error is returned.
     */
    fun check(node: ThirValue): Result<Unit, TypeError> = when (node)
    {
        is ThirCall       -> handle(node)
        is ThirCatch      -> handle(node)
        is ThirLoad       -> handle(node)
        is ThirMember     -> handle(node)
        is ThirRecord     -> Unit.toSuccess()
        is ThirConstBool  -> Unit.toSuccess()
        is ThirConstInt32 -> Unit.toSuccess()
        is ThirConstInt64 -> Unit.toSuccess()
    }
    
    private fun handle(node: ThirCall): Result<Unit, TypeError>
    {
        // Call instances cannot evaluate to an error type.
        if (node.instance.error != null)
            return TypeError.CallContainsError(node.instance).toFailure()
        
        // The instance must evaluate to a callable type.
        // TODO: Support callable structs.
        if (node.instance.value == null)
            return TypeError.CallMissingValue(node.instance).toFailure()
        if (node.instance.value !is ThirType.Function)
            return TypeError.CallWrongType(node.instance).toFailure()
        
        // Parameters are not permitted to have errors.
        node.parameters.firstOrNull { it.error != null }?.also { return TypeError.CallContainsError(it).toFailure() }
        
        // Ensure that all inputs to the call are also valid.
        check(node.instance).onFailure { return it.toFailure() }
        node.parameters.mapUntilError { check(it) }.onFailure { return it.toFailure() }
        return Unit.toSuccess()
    }
    
    private fun handle(node: ThirCatch): Result<Unit, TypeError>
    {
        // Both left-hand and right-hand expression must have a valid value.
        if (node.lhs.value == null && node.capture == Capture.HANDLE)
            return TypeError.CatchMissingValue(node.lhs).toFailure()
        if (node.rhs.value == null)
            return TypeError.CatchMissingValue(node.rhs).toFailure()
        
        // The left-hand side must evaluate to an error, whereas the right-hand side cannot.
        if (node.lhs.error == null)
            return TypeError.CatchMissingError(node.lhs).toFailure()
        if (node.rhs.error != null)
            return TypeError.CatchContainsError(node.rhs).toFailure()
        
        // Ensure all inputs to the catch are also valid.
        check(node.lhs).onFailure { return it.toFailure() }
        check(node.rhs).onFailure { return it.toFailure() }
        return Unit.toSuccess()
    }
    
    private fun handle(node: ThirLoad): Result<Unit, TypeError>
    {
        // TODO: Implement me.
        return Unit.toSuccess()
    }
    
    private fun handle(node: ThirMember): Result<Unit, TypeError>
    {
        // TODO: Implement me.
        return Unit.toSuccess()
    }
}
