package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Assignment
import com.github.derg.transpiler.source.ast.Control
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.util.*

internal class ConverterAssign(private val symbols: SymbolTable)
{
    operator fun invoke(node: Assignment.Assign): Result<Instruction, ResolveError>
    {
        val variable = symbols.resolveRequiredVariable(node.name).valueOr { return failureOf(it) }
        val value = symbols.resolveRequiredValue(node.expression).valueOr { return failureOf(it) }
        
        // TODO: Reject updates to constant and/or immutable variables
        if (variable.type.id != value.type.id)
            return ResolveError.MismatchedVariableType(expected = variable.type, actual = value.type).toFailure()
        return Assign(variable, value).toSuccess()
    }
}

internal class ConverterBranch(private val symbols: SymbolTable)
{
    operator fun invoke(node: Control.Branch): Result<Instruction, ResolveError>
    {
        val predicate = symbols.resolveRequiredValue(node.predicate).valueOr { return failureOf(it) }
        if (predicate.type.id != Builtin.BOOL.id)
            return ResolveError.InvalidPredicateType(predicate.type).toFailure()
        
        return Condition(
            predicate = predicate,
            success = symbols.resolveScope(node.success).valueOr { return failureOf(it) },
            failure = symbols.resolveScope(node.failure).valueOr { return failureOf(it) },
        ).toSuccess()
    }
}

internal class ConverterRaise(private val symbols: SymbolTable)
{
    operator fun invoke(node: Control.Raise): Result<Instruction, ResolveError>
    {
        val value = symbols.resolveRequiredValue(node.expression).valueOr { return failureOf(it) }
        return Raise(value).toSuccess()
    }
}

internal class ConverterReturn(private val symbols: SymbolTable)
{
    operator fun invoke(node: Control.Return): Result<Instruction, ResolveError>
    {
        val value = symbols.resolveOptionalValue(node.expression).valueOr { return failureOf(it) }
        return if (value == null) Exit.toSuccess() else Return(value).toSuccess()
    }
}
