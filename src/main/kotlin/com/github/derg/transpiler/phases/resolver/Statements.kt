package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * The statement converter operates on one statement at a time, converting it into an instruction which represents the
 * initial statement.
 */
internal class ConverterStatements(private val symbols: ThirSymbolTable)
{
    private val expressions = ConverterExpression(symbols)
    
    operator fun invoke(node: AstStatement): Result<ThirInstruction, ResolveError> = when (node)
    {
        is AstAssign      -> convert(node)
        is AstBranch      -> convert(node)
        is AstEvaluate    -> convert(node)
        is AstReturn      -> ThirReturn.toSuccess()
        is AstReturnError -> expressions(node.expression).mapValue { ThirReturnError(it) }
        is AstReturnValue -> expressions(node.expression).mapValue { ThirReturnValue(it) }
        is AstVariable    -> convert(AstAssign(node.name, node.value))
    }
    
    private fun convert(node: AstAssign): Result<ThirInstruction, ResolveError>
    {
        val symbol = resolveVariable(symbols, node.name).valueOr { return it.toFailure() }
        val value = expressions(node.expression).valueOr { return it.toFailure() }
        
        // TODO: Reject constant and/or immutable variable assignment
        // TODO: Reject assignments to function parameters
        
        if (symbol.type != value.valType)
            return ResolveError.MismatchedVariableType(expected = symbol.type, actual = value.valType).toFailure()
        return ThirAssign(symbol.id, value).toSuccess()
    }
    
    private fun convert(node: AstEvaluate): Result<ThirInstruction, ResolveError>
    {
        val expression = expressions(node.expression).valueOr { return it.toFailure() }
        if (expression.valType != Builtin.VOID.id)
            return ResolveError.MismatchedEvaluationType(Builtin.VOID.id, expression.valType).toFailure()
        if (expression.errType != Builtin.VOID.id)
            return ResolveError.MismatchedEvaluationType(Builtin.VOID.id, expression.errType).toFailure()
        return ThirEvaluate(expression).toSuccess()
    }
    
    private fun convert(node: AstBranch): Result<ThirInstruction, ResolveError>
    {
        val predicate = expressions(node.predicate).valueOr { return it.toFailure() }
        if (predicate.valType != Builtin.BOOL.id)
            return ResolveError.MismatchedPredicateType(Builtin.BOOL.id, predicate.valType).toFailure()
        if (predicate.errType != Builtin.VOID.id)
            return ResolveError.MismatchedPredicateType(Builtin.VOID.id, predicate.valType).toFailure()
        
        // Actually resolve the success and failure branches
        val instruction = ThirBranch(predicate, ThirScope(symbols), ThirScope(symbols))
        resolveScope(instruction.success, node.success).onFailure { return it.toFailure() }
        resolveScope(instruction.failure, node.failure).onFailure { return it.toFailure() }
        return instruction.toSuccess()
    }
}
