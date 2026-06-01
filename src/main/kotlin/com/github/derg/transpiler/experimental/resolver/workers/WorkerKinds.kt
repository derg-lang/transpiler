package com.github.derg.transpiler.experimental.resolver.workers

import com.github.derg.transpiler.experimental.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * The kind worker examines a kind which should be assigned to a symbol. Once the worker has been able to resolve all
 * expressions which are part of the kind, it will evaluate the expressions to determine what the actual kind is. The
 * resolution will happen within the provided [scope].
 *
 * The [type] is provided as executable code. After type-checking it, the worker will convert the code into an
 * executable form and run it. Once the kind type has been determined, it will be assigned as a constraint to the given
 * [nodeId].
 */
class WorkerTypeEvaluator(
    private val scope: Scope,
    private val nodeId: UUID,
    private val type: HirExpression,
) : Worker
{
    private val worker = WorkerExpression(scope, type)
    private var generatedConstraints = false
    private var generatedRequirements = false
    
    // NOTE: This worker *IS NOT* associated with either the node nor the expression we are solving. Thus, the id of
    //       this worker has nothing to do with either of those two - we need a completely new and arbitrary id.
    override val id: UUID = UUID.randomUUID()
    
    override fun generateWorkers(): List<Worker>
    {
        return listOf(worker)
    }
    
    override fun process(context: Context): Result<Phase, Outcome>
    {
        if (!generatedRequirements)
        {
            generatedRequirements = true
            return Phase.BlockedBy(setOf(type.id)).toSuccess()
        }
        
        if (!generatedConstraints)
        {
            generatedConstraints = true
            val resolved = evaluate(type, context).valueOr { return it.toFailure() }
            val constraint = Constraint.TypeEq(ThirType.Var(nodeId), resolved)
            return Phase.Constrain(listOf(constraint)).toSuccess()
        }
        
        return Phase.Done.toSuccess()
    }
    
    /**
     * Retrieves the evaluated values associated with the given [type] from the given [context]. The evaluation is
     * assumed to be invoked after the worker is done waiting for the relevant evaluation to be completed.
     */
    private fun evaluate(type: HirExpression, context: Context): Result<ThirType, Outcome>
    {
        val value = worker.expressions[context.environment.getCorrelationId(type.id)]
            ?: throw IllegalStateException("Expression '$type' has not been solved yet!")
        val evaluated = value as? ThirExpression.Canonical
            ?: return Outcome.Placeholder("Expression '$value' is not yet evaluated, must be a canonical form").toFailure()
        val resolved = evaluated as? ThirExpression.Type
            ?: return Outcome.Placeholder("Expression '$type' did not evaluate to a type, received '$value'").toFailure()
        return resolved.raw.toSuccess()
    }
}
