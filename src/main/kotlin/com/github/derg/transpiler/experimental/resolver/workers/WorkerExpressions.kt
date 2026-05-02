package com.github.derg.transpiler.experimental.resolver.workers

import com.github.derg.transpiler.experimental.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * Helper for extracting the correct type for a specific literal. This function is only a helper function, and should be
 * refactored away and removed once literals are handled through enums instead of plain strings. Must be fixed at the AST layer
 * and propagated through the whole stack.
 */
private fun literalToKind(literal: String): ThirKind = when (literal)
{
    INT32_LIT_NAME   -> ThirKind.Value(ThirType.Int32)
    INT64_LIT_NAME   -> ThirKind.Value(ThirType.Int64)
    FLOAT32_LIT_NAME -> ThirKind.Value(ThirType.Float32)
    FLOAT64_LIT_NAME -> ThirKind.Value(ThirType.Float64)
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
) : WorkerBase(node.id)
{
    override val id: UUID
        get() = node.id
    
    override fun generateWorkers(): List<Worker> = when (node)
    {
        is HirExpression.Bool       -> emptyList()
        is HirExpression.Decimal    -> emptyList()
        is HirExpression.Integer    -> emptyList()
        is HirExpression.Text       -> emptyList()
        is HirExpression.Call       -> TODO()
        is HirExpression.Catch      -> TODO()
        is HirExpression.Field      -> TODO()
        is HirExpression.Identifier -> node.typeParameters.map { WorkerExpression(scope, it.second) }
    }
    
    override fun generateConstraints(context: Context): Result<List<Constraint>, Outcome>
    {
        return generateConstraints(context, scope, context.environment[node.id], node)
    }
    
    override fun generateData(context: Context)
    {
    }
}

/**
 * Generates the constraints which are applicable to the node we are currently working on.
 */
private fun generateConstraints(
    context: Context,
    scope: Scope,
    kind: ThirKind,
    node: HirExpression,
): Result<List<Constraint>, Outcome> = when (node)
{
    is HirExpression.Bool       -> listOf(Constraint.KindEq(kind, ThirKind.Value(ThirType.Bool))).toSuccess()
    is HirExpression.Decimal    -> listOf(Constraint.KindEq(kind, literalToKind(node.literal))).toSuccess()
    is HirExpression.Integer    -> listOf(Constraint.KindEq(kind, literalToKind(node.literal))).toSuccess()
    is HirExpression.Text       -> listOf(Constraint.KindEq(kind, ThirKind.Value(ThirType.Str))).toSuccess()
    is HirExpression.Call       -> TODO()
    is HirExpression.Catch      -> TODO()
    is HirExpression.Field      -> TODO()
    is HirExpression.Identifier -> constrainIdentifier(context, scope, kind, node)
}

private fun constrainIdentifier(
    context: Context,
    scope: Scope,
    kind: ThirKind,
    node: HirExpression.Identifier,
): Result<List<Constraint>, Outcome>
{
    val candidates = mutableListOf<List<Constraint>>()
    
    for (candidateId in scope.find(node.name))
    {
        val candidate = context.declarations[candidateId]
            ?: throw IllegalStateException("Could not find candidate '$candidateId' in environment")
        
        when (candidate)
        {
            is ThirDeclaration.Constant      -> TODO()
            is ThirDeclaration.Field         -> TODO()
            is ThirDeclaration.Function      -> TODO()
            is ThirDeclaration.Method        -> TODO()
            is ThirDeclaration.Parameter     -> TODO()
            is ThirDeclaration.Structure     -> candidates += listOf(Constraint.KindEq(kind, ThirKind.Type))
            is ThirDeclaration.TypeParameter -> TODO()
            is ThirDeclaration.Variable      -> TODO()
        }
    }
    
    return when (candidates.size)
    {
        0    -> Outcome.Placeholder("No viable candidate was found for node '$node' in scope").toFailure()
        1    -> candidates.single().toSuccess()
        else -> listOf(Constraint.OneOf(candidates)).toSuccess()
    }
}
