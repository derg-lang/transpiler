package com.github.derg.transpiler.experimental.resolver.workers

import com.github.derg.transpiler.experimental.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * Helper for extracting the correct type for a specific literal. This function is only a helper function, and should be
 * refactored away and removed once literals are handled through enums instead of plain strings. Must be fixed at the AST layer
 * and propagated through the whole stack.
 */
private fun literalToType(literal: String): ThirType = when (literal)
{
    INT32_LIT_NAME   -> ThirType.Int32
    INT64_LIT_NAME   -> ThirType.Int64
    FLOAT32_LIT_NAME -> ThirType.Float32
    FLOAT64_LIT_NAME -> ThirType.Float64
    else             -> throw IllegalArgumentException("Unexpected literal '$literal' - it is not one of the built-in ones")
}

/**
 * The expression worker is responsible for generating the constraints which are applicable to the provided [node]. The worker
 * will also ensure that the [scope] is respected as well, such that it is not possible to access information provided in a
 * far-away part of the codebase.
 */
class WorkerExpression(
    private val scope: Scope,
    private val node: HirExpression,
) : Worker
{
    private var generatedConstraints = false
    
    override fun process(context: Context): Result<Phase, Outcome>
    {
        if (generatedConstraints)
            return Phase.Done.toSuccess()
        
        generatedConstraints = true
        val (type, constraints) = generateConstraints()
        return Phase.Constrain(type, constraints).toSuccess()
    }
    
    private fun generateConstraints(): Pair<ThirType, List<Constraint>> = when (node)
    {
        is HirExpression.Bool       -> ThirType.Bool to emptyList()
        is HirExpression.Decimal    -> literalToType(node.literal) to emptyList()
        is HirExpression.Integer    -> literalToType(node.literal) to emptyList()
        is HirExpression.Text       -> ThirType.Str to emptyList()
        is HirExpression.Call       -> TODO()
        is HirExpression.Catch      -> TODO()
        is HirExpression.Field      -> TODO()
        is HirExpression.Identifier -> TODO()
    }
}
