package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.util.*

internal class ConverterAssign(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstAssign): Result<ThirInstruction, ResolveError>
    {
        val variable = symbols.resolveVariable(node.name).valueOr { return failureOf(it) }
        val value = symbols.resolveValue(node.expression).valueOr { return failureOf(it) }
        
        // TODO: Reject updates to constant and/or immutable variables
        if (variable.type.id != value.type.id)
            return ResolveError.MismatchedVariableType(expected = variable.type, actual = value.type).toFailure()
        return ThirAssign(variable, value).toSuccess()
    }
}

internal class ConverterBranch(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstBranch): Result<ThirInstruction, ResolveError>
    {
        val predicate = symbols.resolveValue(node.predicate).valueOr { return failureOf(it) }
        if (predicate.type.id != Builtin.BOOL.id)
            return ResolveError.InvalidPredicateType(predicate.type).toFailure()
        
        return ThirCondition(
            predicate = predicate,
            success = symbols.resolveScope(node.success).valueOr { return failureOf(it) },
            failure = symbols.resolveScope(node.failure).valueOr { return failureOf(it) },
        ).toSuccess()
    }
}

internal class ConverterReturnError(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstReturnError): Result<ThirInstruction, ResolveError>
    {
        val value = symbols.resolveValue(node.expression).valueOr { return failureOf(it) }
        return ThirReturnError(value).toSuccess()
    }
}

internal class ConverterReturnValue(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstReturnValue): Result<ThirInstruction, ResolveError>
    {
        val value = symbols.resolveOptionalValue(node.expression).valueOr { return failureOf(it) }
        return if (value == null) ThirReturn.toSuccess() else ThirReturnValue(value).toSuccess()
    }
}
