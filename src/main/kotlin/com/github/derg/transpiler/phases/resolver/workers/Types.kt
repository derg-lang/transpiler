package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * Constructs a worker which is capable of resolving the [node] to a proper THIR type. The worker will operate in the
 * given [scope], using the information provided through the given [env].
 */
fun typeDefinerOf(
    evaluator: Evaluator,
    node: HirType,
    env: Environment,
    scope: Scope,
): Worker<ThirType> = when (node)
{
    is HirType.Expression -> TypeExpressionDefiner(evaluator, node, env, scope)
    is HirType.Function   -> TODO("Not yet implemented")
}

/**
 * Converts from a HIR type to a THIR type. The expression which contains information regarding the type is required to
 * be fully resolved.
 */
internal class TypeExpressionDefiner(
    evaluator: Evaluator,
    node: HirType.Expression,
    private val env: Environment,
    scope: Scope,
) : Worker<ThirType>
{
    private val exprWorker = expressionDefinerOf(evaluator, node.value, env, scope, null, true)
    private val interpreter = Interpreter(env)
    
    override fun process(): Result<ThirType, Outcome>
    {
        val expression = exprWorker.process().valueOr { return it.toFailure() }
        val evaluation = evaluate(expression).valueOr { return it.toFailure() }
        
        val type = evaluation as? ThirExpression.Type
            ?: return Outcome.Unhandled("Expression '$expression' evaluated to a non-type value '$evaluation'").toFailure()
        return type.raw.toSuccess()
    }
    
    private fun evaluate(expression: ThirExpression): Result<ThirExpression.Canonical, Outcome>
    {
        if (expression is ThirExpression.Load)
        {
            val symbol = env.declarations[expression.symbolId]
                ?: return Outcome.RequireDeclaration(setOf(expression.symbolId)).toFailure()
            val typeParameter = symbol as? ThirDeclaration.TypeParameter
            if (typeParameter != null)
                return ThirExpression.Type(ThirType.TypeParameterRef(symbol.id)).toSuccess()
            val parameter = symbol as? ThirDeclaration.Parameter
            if (parameter != null)
                return ThirExpression.Type(ThirType.TypeParameterRef(symbol.id)).toSuccess()
        }
        
        val evaluation = interpreter.evaluate(expression)
            .valueOr { return Outcome.Unhandled("Expression '$expression' evaluated to error '$it'").toFailure() }
            ?: return Outcome.Unhandled("Expression '$expression' did not evaluate to anything").toFailure()
        
        return evaluation.toSuccess()
    }
}

/**
 * Converts from a HIR type kind to a THIR type kind.
 */
internal class KindDefiner(
    private val evaluator: Evaluator,
    private val node: HirKind,
    private val env: Environment,
    private val scope: Scope,
) : Worker<ThirKind>
{
    private var worker: Worker<ThirType>? = null
    private var type: ThirType? = null
    
    override fun process(): Result<ThirKind, Outcome>
    {
        worker = when (node)
        {
            is HirKind.Nothing -> return ThirKind.Nothing.toSuccess()
            is HirKind.Type    -> return ThirKind.Type.toSuccess()
            is HirKind.Value   -> worker ?: typeDefinerOf(evaluator, node.type, env, scope)
        }
        if (type == null)
            type = worker!!.process().valueOr { return it.toFailure() }
        
        return ThirKind.Value(type!!).toSuccess()
    }
}
