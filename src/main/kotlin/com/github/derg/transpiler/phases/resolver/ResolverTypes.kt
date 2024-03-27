package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * The type resolver is responsible for deducing the typed info for objects found in the given [scope]. During
 * resolution, no assumptions are required. In order to resolve a type properly, the struct with the appropriate name
 * must be visible from the scope.
 */
internal class ResolverType(private val scope: Scope)
{
    /**
     * Resolves the [type] information to the typed representation if possible. Types may only be resolved if the
     * referenced structs are visible in the current scope.
     */
    fun resolve(type: HirType): Result<ThirType, ResolveError> = when (type)
    {
        is HirTypeCall -> resolve(type)
        is HirTypeData -> resolve(type)
    }
    
    fun resolve(type: HirTypeData): Result<ThirTypeData, ResolveError>
    {
        // TODO: This way of resolving the typed information does not take generics into consideration. We need to match
        //       all provided generics towards all potential candidates, taking names and ordering into consideration.
        val candidates = scope.resolve<HirStruct>(type.name)
        val candidate = when (candidates.size)
        {
            1    -> candidates.single()
            0    -> return ResolveError.UnknownStruct(type.name).toFailure()
            else -> return ResolveError.AmbiguousStruct(type.name).toFailure()
        }
        
        return ThirTypeData(symbolId = candidate.id, generics = emptyList(), mutability = type.mutability).toSuccess()
    }
    
    fun resolve(type: HirTypeCall): Result<ThirTypeCall, ResolveError>
    {
        val value = type.value?.let { resolve(it) }?.valueOr { return it.toFailure() }
        val error = type.error?.let { resolve(it) }?.valueOr { return it.toFailure() }
        val parameters = type.parameters.mapUntilError { handle(it) }.valueOr { return it.toFailure() }
        
        return ThirTypeCall(value = value, error = error, parameters = parameters).toSuccess()
    }
    
    private fun handle(type: Named<HirType>): Result<Named<ThirType>, ResolveError>
    {
        return resolve(type.second).mapValue { type.first to it }
    }
}
