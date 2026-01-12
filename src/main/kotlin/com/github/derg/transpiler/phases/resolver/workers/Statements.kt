package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.interpreter.*
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
    evaluator: Evaluator,
    node: HirStatement,
    env: Environment,
    scope: Scope,
): Worker<ThirStatement> = when (node)
{
    is HirStatement.Assign      -> AssignDefiner(evaluator, node, env, scope)
    is HirStatement.Evaluate    -> EvaluateDefiner(evaluator, node, env, scope)
    is HirStatement.For         -> TODO("Not yet implemented")
    is HirStatement.If          -> IfDefiner(evaluator, node, env, scope)
    is HirStatement.Return      -> ReturnDefiner
    is HirStatement.ReturnError -> ReturnErrorDefiner(evaluator, node, env, scope)
    is HirStatement.ReturnValue -> ReturnValueDefiner(evaluator, node, env, scope)
    is HirStatement.Variable    -> VariableDefiner(evaluator, node, env, scope)
    is HirStatement.While       -> WhileDefiner(evaluator, node, env, scope)
}

/**
 * Issues an initialization statement for the variable node.
 */
internal class AssignDefiner(
    private val evaluator: Evaluator,
    private val node: HirStatement.Assign,
    private val env: Environment,
    private val scope: Scope,
) : Worker<ThirStatement>
{
    private val instanceWorker: Worker<ThirExpression> = expressionDefinerOf(evaluator, node.instance, env, scope, null, false)
    private var expressionWorker: Worker<ThirExpression>? = null
    
    private var instance: ThirExpression? = null
    private var expression: ThirExpression? = null
    
    override fun process(): Result<ThirStatement, Outcome>
    {
        if (instance == null)
            instance = instanceWorker.process().valueOr { return it.toFailure() }
        if (expressionWorker == null)
            expressionWorker = expressionDefinerOf(evaluator, node.expression, env, scope, instance!!.valueKind, false)
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
internal class EvaluateDefiner(
    evaluator: Evaluator,
    node: HirStatement.Evaluate,
    env: Environment,
    scope: Scope,
) : Worker<ThirStatement>
{
    private val worker = expressionDefinerOf(evaluator, node.expression, env, scope, null, false)
    
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
internal class IfDefiner(
    evaluator: Evaluator,
    node: HirStatement.If,
    env: Environment,
    scope: Scope,
) : Worker<ThirStatement>
{
    private val innerScopeTrue = Scope(scope)
    private val innerScopeFalse = Scope(scope)
    private val predicateWorker = expressionDefinerOf(evaluator, node.predicate, env, scope, ThirKind.Value(ThirType.Bool), false)
    private val successWorker = WorkerList(node.success) { statementDefinerOf(evaluator, it, env, innerScopeTrue) }
    private val failureWorker = WorkerList(node.failure) { statementDefinerOf(evaluator, it, env, innerScopeFalse) }
    
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
internal class ReturnErrorDefiner(
    evaluator: Evaluator,
    node: HirStatement.ReturnError,
    env: Environment,
    scope: Scope,
) : Worker<ThirStatement>
{
    private val worker = expressionDefinerOf(evaluator, node.expression, env, scope, null, false)
    
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
internal class ReturnValueDefiner(
    evaluator: Evaluator,
    node: HirStatement.ReturnValue,
    env: Environment,
    scope: Scope,
) : Worker<ThirStatement>
{
    private val worker = expressionDefinerOf(evaluator, node.expression, env, scope, null, false)
    
    override fun process(): Result<ThirStatement, Outcome>
    {
        val expr = worker.process().valueOr { return it.toFailure() }
        if (expr.errorKind !is ThirKind.Nothing)
            return Outcome.ReturnHasError(expr.errorKind).toFailure()
        
        return worker.process().mapValue { ThirStatement.ReturnValue(it) }
    }
}

/**
 * Issues an initialization statement for the variable node.
 */
internal class VariableDefiner(
    evaluator: Evaluator,
    private val node: HirStatement.Variable,
    private val env: Environment,
    private val scope: Scope,
) : Worker<ThirStatement>
{
    private val worker = TypeExprResolver(evaluator, node.kind, node.value, env, scope, true)
    
    override fun process(): Result<ThirStatement, Outcome>
    {
        // When processing a variable, we are in the context of processing statements. Thus, we must ensure that the
        // variable is fully defined during this process.
        val type = worker.resolveDeclaration().valueOr { return it.toFailure() }
        val value = worker.resolveDefinition().valueOr { return it.toFailure() }
        val symbol = ThirDeclaration.Variable(node.id, node.name, node.assignability, type, ThirDeclaration.VariableDef(value!!))
        
        env.declarations[node.id] = symbol
        scope.register(node.id, node.name)

        val instance = ThirExpression.Load(node.id, symbol.kind)
        return ThirStatement.Assign(instance, symbol.def!!.value).toSuccess()
    }
}

/**
 * Converts from a HIR if statement to a THIR statement.
 */
internal class WhileDefiner(
    evaluator: Evaluator,
    node: HirStatement.While,
    env: Environment,
    scope: Scope,
) : Worker<ThirStatement>
{
    private val innerScope = Scope(scope)
    private val predicateWorker = expressionDefinerOf(evaluator, node.predicate, env, scope, null, false)
    private val statementsWorker = WorkerList(node.body) { statementDefinerOf(evaluator, it, env, innerScope) }
    
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
