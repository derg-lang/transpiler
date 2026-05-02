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
 * The [kind] is provided as executable code. After type-checking it, the worker will convert the code into an
 * executable form and run it. Once the kind type has been determined, it will be assigned as a constraint to the given
 * [nodeId].
 */
class WorkerKindEvaluator(
    private val scope: Scope,
    private val nodeId: UUID,
    private val kind: HirKind,
) : Worker
{
    private val expressions = extract(kind)
    private var generatedConstraints = false
    private var generatedRequirements = false
    private var generatedSpawns = false
    
    override val id: UUID = UUID.randomUUID()
    
    override fun process(context: Context): Result<Phase, Outcome>
    {
        // Any auto-deduction stuff can be ignored in this section. The point is to evaluate the kind provided by the
        // user by running the code at compile-time. If the user has asked us to deduce the kind, then the type system
        // can handle it without compile-time code execution.
        if (kind == HirKind.Deduce)
            return Phase.Done.toSuccess()
        
        if (!generatedSpawns)
        {
            generatedSpawns = true
            val workers = expressions.map { WorkerExpression(scope, it) }
            return Phase.Spawn(workers).toSuccess()
        }
        
        // Whenever we want to resolve a kind which is user-provided,
        if (!generatedRequirements)
        {
            generatedRequirements = true
            val ids = expressions.map { it.id }.toSet()
            return Phase.BlockedBy(ids).toSuccess()
        }
        
        if (!generatedConstraints)
        {
            generatedConstraints = true
            val resolved = evaluate(kind, context).valueOr { return it.toFailure() }
            val constraint = Constraint.KindEq(ThirKind.Var(nodeId), resolved)
            return Phase.Constrain(listOf(constraint)).toSuccess()
        }
        
        return Phase.Done.toSuccess()
    }
    
    /**
     * Retrieves the list of expressions which are providing the value of whatever [kind] is. All these expressions
     * together must be evaluated to determine what the kind actually is.
     */
    private fun extract(kind: HirKind): List<HirExpression> = when (kind)
    {
        is HirKind.Deduce  -> emptyList()
        is HirKind.Nothing -> emptyList()
        is HirKind.Type    -> emptyList()
        is HirKind.Value   -> extract(kind.type)
    }
    
    private fun extract(type: HirType): List<HirExpression> = when (type)
    {
        is HirType.Expression -> listOf(type.value)
        is HirType.Function   -> (listOfNotNull(type.valueKind, type.errorKind) + type.parameters).flatMap { extract(it) }
    }
    
    /**
     * Retrieves the evaluated values associated with the given [kind] from the given [context]. The evaluation is
     * assumed to be invoked after the worker is done waiting for the relevant evaluation to be completed.
     */
    private fun evaluate(kind: HirKind, context: Context): Result<ThirKind, Outcome> = when (kind)
    {
        is HirKind.Deduce  -> throw IllegalStateException("Illegal kind, the auto kind is not permitted to appear here")
        is HirKind.Nothing -> ThirKind.Nothing.toSuccess()
        is HirKind.Type    -> ThirKind.Type.toSuccess()
        is HirKind.Value   -> evaluate(kind.type, context).mapValue { ThirKind.Value(it) }
    }
    
    private fun evaluate(type: HirType, context: Context): Result<ThirType, Outcome> = when (type)
    {
        is HirType.Expression -> evaluate(type.value, context)
        is HirType.Function   ->
        {
            val valueKind = evaluate(type.valueKind, context).valueOr { return it.toFailure() }
            val errorKind = evaluate(type.errorKind, context).valueOr { return it.toFailure() }
            val parameters = type.parameters.mapUntilError { evaluate(it, context) }.valueOr { return it.toFailure() }
            ThirType.Function(parameters, valueKind, errorKind).toSuccess()
        }
    }
    
    private fun evaluate(expression: HirExpression, context: Context): Result<ThirType, Outcome>
    {
        val value = context.evaluations[expression.id]
            ?: throw IllegalStateException("Expression '$expression' has not been evaluated yet!")
        val type = value as? ThirExpression.Type
            ?: return Outcome.Placeholder("Expression '$expression' did not evaluate to a type, received '$value'").toFailure()
        return type.raw.toSuccess()
    }
}
