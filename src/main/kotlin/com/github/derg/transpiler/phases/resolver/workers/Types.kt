package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * Constructs a worker which is capable of resolving the [node] to a proper THIR type. The worker will operate in the
 * given [scope], using the information provided through the given [env].
 */
fun typeDefinerOf(
    node: HirType,
    env: Environment,
    scope: Scope,
): Worker<ThirType> = when (node)
{
    is HirType.Expression -> TypeExpressionDefiner(node, env, scope)
    is HirType.Function   -> TODO("Not yet implemented")
}

/**
 * Converts from a HIR type to a THIR type. The expression which contains information regarding the type is required to
 * be fully resolved.
 */
internal class TypeExpressionDefiner(node: HirType.Expression, env: Environment, scope: Scope) : Worker<ThirType>
{
    private val exprWorker = expressionDefinerOf(node.value, env, scope, null, true)
    private val interpreter = Interpreter(env)
    
    override fun process(): Result<ThirType, Outcome>
    {
        val expression = exprWorker.process().valueOr { return it.toFailure() }
        val evaluation = evaluate(expression).valueOr { return it.toFailure() }
        
        val type = evaluation as? ThirExpression.Type
            ?: return Outcome.Unhandled("Expression '$expression' evaluated to a non-type value '$evaluation'").toFailure()
        return type.raw.toSuccess()
    }
    
    private fun evaluate(expression: ThirExpression): Result<ThirExpression, Outcome>
    {
        val evaluation = interpreter.evaluate(expression)
            .valueOr { return Outcome.Unhandled("Expression '$expression' evaluated to error '$it'").toFailure() }
            ?: return Outcome.Unhandled("Expression '$expression' did not evaluate to anything").toFailure()
        
        return when (evaluation)
        {
            is ThirExpression.Call -> evaluate(evaluation)
            is ThirExpression.Load -> evaluate(evaluation)
            else                   -> evaluation.toSuccess()
        }
    }
}
