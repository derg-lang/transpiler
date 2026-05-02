package com.github.derg.transpiler.experimental.resolver.workers

import com.github.derg.transpiler.experimental.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.utils.*

/**
 * The module worker is responsible for working on everything which is present within the given module [node]. This worker
 * ensures that all child workers required to process the module are fully spawned.
 */
class WorkerModule(
    private val packageScope: Scope,
    private val node: HirDeclaration.ModuleDecl,
) : WorkerBase(node.id)
{
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
    
    override fun generateConstraints(context: Context): Result<List<Constraint>, Outcome>
    {
        return Success(emptyList())
    }
    
    override fun generateData(context: Context)
    {
    }
}

/**
 * The constant worker ensures that all applicable constraints for a constant is generated and available during type
 * inference.
 */
class WorkerConstant(
    private val scope: Scope,
    private val node: HirDeclaration.ConstantDecl,
) : WorkerBase(node.id)
{
    override fun generateWorkers(): List<Worker>
    {
        val kindWorker = WorkerKindEvaluator(scope, node.id, node.kind)
        val valueWorker = WorkerExpression(scope, node.value)
        return listOf(kindWorker, valueWorker)
    }
    
    override fun generateConstraints(context: Context): Result<List<Constraint>, Outcome>
    {
        val kind = context.environment[node.id]
        val constraint = Constraint.KindEq(kind, ThirKind.Var(node.value.id))
        return listOf(constraint).toSuccess()
    }
    
    override fun generateData(context: Context)
    {
        context.declarations[node.id] = ThirDeclaration.Constant(
            id = node.id,
            name = node.name,
            kind = ThirKind.Var(node.id),
            def = null,
        )
    }
}
