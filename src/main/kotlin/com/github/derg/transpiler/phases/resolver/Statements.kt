package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Assignment
import com.github.derg.transpiler.source.hir.Assign
import com.github.derg.transpiler.source.hir.Instruction
import com.github.derg.transpiler.source.hir.SymbolTable
import com.github.derg.transpiler.util.*

class ConverterAssign(private val symbols: SymbolTable)
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
