package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * The instruction resolver is responsible for ensuring that all instructions within the given [scope] are resolved to
 * typed instructions. Resolution depends on the [types] which are available in the type table. Note that resolution
 * cannot take place unless all types that are visible from the scope have been resolved and recorded in the type table.
 *
 * All instruction resolution takes place before the symbol table has been constructed. This implies that the symbol
 * table cannot be used while resolving instructions.
 */
internal class ResolverInstruction(private val types: TypeTable, private val scope: Scope)
{
    private val values = ResolverValue(types, scope)
    
    /**
     * Converts the instruction indicated by the [node] to a typed version. This operation converts all raw names into
     * the actual id of the objects referenced where possible. If type conversion fails, an error describing the cause
     * of failure will be returned.
     */
    fun resolve(node: HirInstruction): Result<ThirInstruction, ResolveError> = when (node)
    {
        is HirAssign      -> handle(node)
        is HirBranch      -> handle(node)
        is HirEvaluate    -> handle(node)
        is HirReturn      -> ThirReturn.toSuccess()
        is HirReturnError -> values.resolve(node.value).mapValue { ThirReturnError(it) }
        is HirReturnValue -> values.resolve(node.value).mapValue { ThirReturnValue(it) }
    }
    
    private fun handle(node: HirAssign): Result<ThirInstruction, ResolveError>
    {
        // TODO: Support variable assignments that are loaded from arbitrary locations in memory.
        if (node.instance !is HirLoad)
            return ResolveError.Placeholder.toFailure()
        
        val candidates = scope.resolve<HirVariable>(node.instance.name)
        val candidate = when (candidates.size)
        {
            1    -> candidates.single()
            0    -> return ResolveError.UnknownVariable(node.instance.name).toFailure()
            else -> return ResolveError.AmbiguousVariable(node.instance.name, node.value).toFailure()
        }
        
        val value = values.resolve(node.value).valueOr { return it.toFailure() }
        return ThirAssign(candidate.id, value).toSuccess()
    }
    
    private fun handle(node: HirBranch): Result<ThirInstruction, ResolveError>
    {
        // TODO: Create a new scope for each branch.
        val predicate = values.resolve(node.predicate).valueOr { return it.toFailure() }
        val success = node.success.mapUntilError { resolve(it) }.valueOr { return it.toFailure() }
        val failure = node.failure.mapUntilError { resolve(it) }.valueOr { return it.toFailure() }
        
        return ThirBranch(predicate, success, failure).toSuccess()
    }
    
    private fun handle(node: HirEvaluate): Result<ThirInstruction, ResolveError>
    {
        val expression = values.resolve(node.expression).valueOr { return it.toFailure() }
        
        return ThirEvaluate(expression).toSuccess()
    }
}
