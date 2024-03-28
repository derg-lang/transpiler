package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * Performs type-checking on the instruction [node]. If there are any type errors detected, an error is returned.
 */
internal fun check(node: ThirInstruction): Result<Unit, TypeError> = when (node)
{
    is ThirAssign      -> TODO()
    is ThirBranch      -> handle(node)
    is ThirEvaluate    -> handle(node)
    is ThirReturn      -> TODO()
    is ThirReturnError -> TODO()
    is ThirReturnValue -> TODO()
}

private fun handle(node: ThirBranch): Result<Unit, TypeError>
{
    // Predicates are not permitted to contain error types.
    if (node.predicate.error != null)
        return TypeError.BranchPredicateHasError(node.predicate).toFailure()
    
    // The value type must be boolean.
    val value = node.predicate.value as? ThirTypeData
    if (value == null || value.symbolId != Builtin.BOOL.id)
        return TypeError.BranchPredicateNotBool(node.predicate).toFailure()
    
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
    
    return Unit.toSuccess()
}
