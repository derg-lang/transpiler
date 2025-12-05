package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
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
internal class AssignDefiner(
    private val node: HirStatement.Assign,
    private val env: Environment,
    private val scope: Scope,
) : Worker<ThirStatement>
{
    private val instanceWorker: Worker<ThirExpression> = expressionDefinerOf(node.instance, env, scope, null, false)
    private var expressionWorker: Worker<ThirExpression>? = null
    
    private var instance: ThirExpression? = null
    private var expression: ThirExpression? = null
    
    override fun process(): Result<ThirStatement, Outcome>
    {
        if (instance == null)
            instance = instanceWorker.process().valueOr { return it.toFailure() }
        if (expressionWorker == null)
            expressionWorker = expressionDefinerOf(node.expression, env, scope, instance!!.valueKind, false)
        if (expression == null)
            expression = expressionWorker!!.process().valueOr { return it.toFailure() }
        
        val instance = instance!!
        if (instance is ThirExpression.Load)
        {
            val symbol = env.declarations[instance.symbolId]!!
            if (symbol !is ThirDeclaration.Variable)
                return Outcome.SymbolNotAssignable(symbol.name).toFailure()
            if (symbol.assignability != Assignability.ASSIGNABLE)
                return Outcome.VariableNotAssignable(symbol.name).toFailure()
        }
        
        return ThirStatement.Assign(instance, expression!!).toSuccess()
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
        if (expr.valueKind != ThirKind.Nothing)
            return Outcome.EvaluationHasValue(expr.valueKind).toFailure()
        if (expr.errorKind != ThirKind.Nothing)
            return Outcome.EvaluationHasError(expr.errorKind).toFailure()
        
        return ThirStatement.Evaluate(expr).toSuccess()
    }
}

/**
 * Converts from a HIR if statement to a THIR statement.
 */
internal class IfDefiner(node: HirStatement.If, env: Environment, scope: Scope) : Worker<ThirStatement>
{
    private val predicateWorker = expressionDefinerOf(node.predicate, env, scope, ThirKind.Value(ThirType.Bool), false)
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
            val kind = expression.valueKind as? ThirKind.Value
                ?: return Outcome.PredicateWrongKind(expression.valueKind).toFailure()
            if (kind.type != ThirType.Bool)
                return Outcome.PredicateWrongType(kind.type).toFailure()
            if (expression.errorKind !is ThirKind.Nothing)
                return Outcome.PredicateHasError(expression.errorKind).toFailure()
            
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
        
        val instance = ThirExpression.Load(node.id, symbol.kind)
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
        if (expr.errorKind !is ThirKind.Nothing)
            return Outcome.ReturnHasError(expr.errorKind).toFailure()
        
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
        if (expr.errorKind !is ThirKind.Nothing)
            return Outcome.ReturnHasError(expr.errorKind).toFailure()
        
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
            val kind = expression.valueKind as? ThirKind.Value
                ?: return Outcome.PredicateWrongKind(expression.valueKind).toFailure()
            if (kind.type != ThirType.Bool)
                return Outcome.PredicateWrongType(kind.type).toFailure()
            if (expression.errorKind !is ThirKind.Nothing)
                return Outcome.PredicateHasError(expression.errorKind).toFailure()
            
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
