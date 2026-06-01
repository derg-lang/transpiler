package com.github.derg.transpiler.experimental.resolver.workers

import com.github.derg.transpiler.experimental.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * The module worker is responsible for working on everything which is present within the given module [node]. This worker
 * ensures that all child workers required to process the module are fully spawned.
 */
class WorkerModule(
    private val packageScope: Scope,
    private val node: HirDeclaration.ModuleDecl,
) : Worker
{
    override val id: UUID
        get() = node.id
    
    override fun generateWorkers(): List<Worker>
    {
        val scope = Scope(packageScope)
        val children = mutableListOf<Worker>()
        for (segment in node.segments)
        {
            children += segment.constants.map { WorkerConstant(scope, it) }
        }
        return children
    }
}

/**
 * The constant worker ensures that all applicable constraints for a constant is generated and available during type
 * inference.
 */
class WorkerConstant(
    private val scope: Scope,
    private val node: HirDeclaration.ConstantDecl,
) : Worker
{
    override val id: UUID
        get() = node.id
    
    override fun generateDeclaration() = ThirDeclaration.Constant(
        id = node.id,
        name = node.name,
        def = null,
    )
    
    override fun generateWorkers(): List<Worker>
    {
        val kindWorker = if (node.kind is HirKind.Value) WorkerTypeEvaluator(scope, node.id, node.kind.type) else null
        val valueWorker = WorkerExpression(scope, node.value)
        return listOfNotNull(kindWorker, valueWorker)
    }
    
    override fun generateConstraints(context: Context): Result<List<Constraint>, Outcome>
    {
        // Constants are not permitted to hold errors - explicitly state that the type cannot hold errors.
        val type = ThirType.Channel(context.environment[node.id], ThirType.Void)
        
        val typeConstraint = Constraint.TypeEq(type, ThirType.Var(node.value.id))
        val kindConstraint = when (node.kind)
        {
            is HirKind.Deduce  -> null
            is HirKind.Value   -> Constraint.TypeEq(context.environment[node.kind.type.id], ThirType.Type)
            is HirKind.Nothing -> return Outcome.Placeholder("Cannot declare constant '$node' with a nothing kind").toFailure()
            is HirKind.Type    -> return Outcome.Placeholder("Cannot declare constant '$node' with a type kind").toFailure()
        }
        return listOfNotNull(typeConstraint, kindConstraint).toSuccess()
    }
}
