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
     * Resolves the [input] information to the typed representation if possible. Types may only be resolved if the
     * referenced structs are visible in the current scope.
     */
    fun resolve(input: HirType): Result<ThirType, ResolveError> = when (input)
    {
        is HirTypeFunction -> resolve(input)
        is HirTypeLiteral  -> resolve(input)
        is HirTypeStruct   -> resolve(input)
        is HirTypeUnion    -> resolve(input)
    }
    
    fun resolve(input: HirTypeStruct): Result<ThirTypeStruct, ResolveError>
    {
        // TODO: This way of resolving the typed information does not take generics into consideration. We need to match
        //       all provided generics towards all potential candidates, taking names and ordering into consideration.
        val candidates = scope.resolve<HirStruct>(input.name)
        val candidate = when (candidates.size)
        {
            1    -> candidates.single()
            0    -> return ResolveError.UnknownStruct(input.name).toFailure()
            else -> return ResolveError.AmbiguousStruct(input.name).toFailure()
        }
        
        return ThirTypeStruct(symbolId = candidate.id, generics = emptyList(), mutability = input.mutability).toSuccess()
    }
    
    fun resolve(input: HirTypeFunction): Result<ThirTypeFunction, ResolveError>
    {
        val value = input.value?.let { resolve(it) }?.valueOr { return it.toFailure() }
        val error = input.error?.let { resolve(it) }?.valueOr { return it.toFailure() }
        val parameters = input.parameters.mapUntilError { resolve(it) }.valueOr { return it.toFailure() }
        
        return ThirTypeFunction(value = value, error = error, parameters = parameters).toSuccess()
    }
    
    fun resolve(input: HirTypeLiteral): Result<ThirTypeLiteral, ResolveError>
    {
        val value = resolve(input.value).valueOr { return it.toFailure() }
        val parameter = resolve(input.parameter).valueOr { return it.toFailure() }
        
        return ThirTypeLiteral(value = value, parameter = parameter).toSuccess()
    }
    
    fun resolve(input: HirTypeUnion): Result<ThirType, ResolveError>
    {
        val types = input.types.mapUntilError { resolve(it) }.valueOr { return it.toFailure() }
        
        return ThirTypeUnion(types).toSuccess()
    }
    
    private fun resolve(input: HirTypeFunction.Parameter): Result<ThirTypeFunction.Parameter, ResolveError>
    {
        val type = resolve(input.type).valueOr { return it.toFailure() }
        
        return ThirTypeFunction.Parameter(input.name, type).toSuccess()
    }
}
