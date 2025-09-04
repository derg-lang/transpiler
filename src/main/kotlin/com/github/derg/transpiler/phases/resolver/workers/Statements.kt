package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * Constructs a worker which is capable of resolving the [node] to a proper THIR statement. The worker will operate in
 * the given [scope], using the information provided through the given [env].
 */
fun statementDefinerOf(
    node: HirStatement,
    env: Environment,
    scope: Scope,
): Worker<ThirStatement> = when (node)
{
    is HirStatement.Assign      -> AssignDefiner(node, env, scope)
    is HirStatement.Evaluate    -> EvaluateDefiner(node, env, scope)
    is HirStatement.For         -> TODO("Not yet implemented")
    is HirStatement.If          -> IfDefiner(node, env, scope)
    is HirStatement.Return      -> ReturnDefiner
    is HirStatement.ReturnError -> ReturnErrorDefiner(node, env, scope)
    is HirStatement.ReturnValue -> ReturnValueDefiner(node, env, scope)
    is HirStatement.Variable    -> InitializeDefiner(node, env, scope)
    is HirStatement.While       -> WhileDefiner(node, env, scope)
}

/**
 * Issues an initialization statement for the variable node.
 */
internal class AssignDefiner(node: HirStatement.Assign, env: Environment, scope: Scope) : Worker<ThirStatement>
{
    override fun process(): Result<ThirStatement, Outcome>
    {
        TODO("Not yet implemented")
    }
}

/**
 * Converts from a HIR evaluation statement to a THIR statement.
 */
internal class EvaluateDefiner(node: HirStatement.Evaluate, env: Environment, scope: Scope) : Worker<ThirStatement>
{
    private val worker = expressionDefinerOf(node.expression, env, scope, null, false)
    
    override fun process(): Result<ThirStatement, Outcome>
    {
        val expr = worker.process().valueOr { return it.toFailure() }
        if (expr.valueType != ThirType.Void)
            return Outcome.MismatchedType(expected = ThirType.Void, received = expr.valueType).toFailure()
        if (expr.errorType != ThirType.Void)
            return Outcome.MismatchedType(expected = ThirType.Void, received = expr.errorType).toFailure()
        
        return ThirStatement.Evaluate(expr).toSuccess()
    }
}

/**
 * Converts from a HIR if statement to a THIR statement.
 */
internal class IfDefiner(node: HirStatement.If, env: Environment, scope: Scope) : Worker<ThirStatement>
{
    private val predicateWorker = expressionDefinerOf(node.predicate, env, scope, ThirType.Bool, false)
    private val successWorker = WorkerList(node.success) { statementDefinerOf(it, env, scope) }
    private val failureWorker = WorkerList(node.failure) { statementDefinerOf(it, env, scope) }
    
    private var predicate: ThirExpression? = null
    private var success: List<ThirStatement>? = null
    private var failure: List<ThirStatement>? = null
    
    override fun process(): Result<ThirStatement, Outcome>
    {
        if (predicate == null)
        {
            val expression = predicateWorker.process().valueOr { return it.toFailure() }
            if (expression.valueType != ThirType.Bool)
                return Outcome.MismatchedType(expected = ThirType.Bool, received = expression.valueType).toFailure()
            if (expression.errorType != ThirType.Void)
                return Outcome.MismatchedType(expected = ThirType.Void, received = expression.errorType).toFailure()
            
            predicate = expression
        }
        if (success == null)
            success = successWorker.process().valueOr { return it.toFailure() }
        if (failure == null)
            failure = failureWorker.process().valueOr { return it.toFailure() }
        
        return ThirStatement.If(
            predicate = predicate!!,
            success = success!!,
            failure = failure!!,
        ).toSuccess()
    }
}

/**
 * Issues an initialization statement for the variable node.
 */
internal class InitializeDefiner(
    private val node: HirStatement.Variable,
    private val env: Environment,
    private val scope: Scope,
) : Worker<ThirStatement>
{
    override fun process(): Result<ThirStatement, Outcome>
    {
        val symbol = env.declarations[node.id] as? ThirDeclaration.Variable
            ?: return Outcome.RequireDefinition(setOf(node.id)).toFailure()
        
        scope.register(node.id, node.name)
        
        val instance = ThirExpression.Load(node.id, symbol.type)
        return ThirStatement.Assign(instance, symbol.def!!.value).toSuccess()
    }
}

/**
 * Converts from a HIR return to a THIR return.
 */
internal object ReturnDefiner : Worker<ThirStatement>
{
    override fun process(): Result<ThirStatement, Outcome> =
        ThirStatement.Return.toSuccess()
}

/**
 * Converts from a HIR error return to a THIR statement.
 */
internal class ReturnErrorDefiner(node: HirStatement.ReturnError, env: Environment, scope: Scope) : Worker<ThirStatement>
{
    private val worker = expressionDefinerOf(node.expression, env, scope, null, false)
    
    override fun process(): Result<ThirStatement, Outcome>
    {
        val expr = worker.process().valueOr { return it.toFailure() }
        if (expr.errorType != ThirType.Void)
            return Outcome.MismatchedType(expected = ThirType.Void, received = expr.errorType).toFailure()
        
        return worker.process().mapValue { ThirStatement.ReturnError(it) }
    }
}

/**
 * Converts from a HIR value return to a THIR statement.
 */
internal class ReturnValueDefiner(node: HirStatement.ReturnValue, env: Environment, scope: Scope) : Worker<ThirStatement>
{
    private val worker = expressionDefinerOf(node.expression, env, scope, null, false)
    
    override fun process(): Result<ThirStatement, Outcome>
    {
        val expr = worker.process().valueOr { return it.toFailure() }
        if (expr.errorType != ThirType.Void)
            return Outcome.MismatchedType(expected = ThirType.Void, received = expr.errorType).toFailure()
        
        return worker.process().mapValue { ThirStatement.ReturnValue(it) }
    }
}

/**
 * Converts from a HIR if statement to a THIR statement.
 */
internal class WhileDefiner(node: HirStatement.While, env: Environment, scope: Scope) : Worker<ThirStatement>
{
    private val predicateWorker = expressionDefinerOf(node.predicate, env, scope, null, false)
    private val statementsWorker = WorkerList(node.body) { statementDefinerOf(it, env, scope) }
    
    private var predicate: ThirExpression? = null
    private var statements: List<ThirStatement>? = null
    
    override fun process(): Result<ThirStatement, Outcome>
    {
        if (predicate == null)
        {
            val expression = predicateWorker.process().valueOr { return it.toFailure() }
            if (expression.valueType != ThirType.Bool)
                return Outcome.MismatchedType(expected = ThirType.Bool, received = expression.valueType).toFailure()
            if (expression.errorType != ThirType.Void)
                return Outcome.MismatchedType(expected = ThirType.Void, received = expression.errorType).toFailure()
            
            predicate = expression
        }
        if (statements == null)
            statements = statementsWorker.process().valueOr { return it.toFailure() }
        
        return ThirStatement.While(
            predicate = predicate!!,
            statements = statements!!,
        ).toSuccess()
    }
}
