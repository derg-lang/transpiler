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
        is HirType.Function  -> resolve(type)
        is HirType.Literal   -> resolve(type)
        is HirType.Structure -> resolve(type)
        is HirType.Union     -> resolve(type)
    }
    
    fun resolve(type: HirType.Structure): Result<ThirTypeStruct, ResolveError>
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
        
        return ThirTypeStruct(symbolId = candidate.id, generics = emptyList(), mutability = type.mutability).toSuccess()
    }
    
    fun resolve(type: HirType.Function): Result<ThirTypeFunction, ResolveError>
    {
        val value = type.value?.let { resolve(it) }?.valueOr { return it.toFailure() }
        val error = type.error?.let { resolve(it) }?.valueOr { return it.toFailure() }
        val parameters = type.parameters.mapUntilError { resolve(it) }.valueOr { return it.toFailure() }
        
        return ThirTypeFunction(value = value, error = error, parameters = parameters).toSuccess()
    }

    fun resolve(type: HirType.Literal): Result<ThirTypeLiteral, ResolveError>
    {
        val value = resolve(type.value).valueOr { return it.toFailure() }
        val parameter = resolve(type.parameter).valueOr { return it.toFailure() }

        return ThirTypeLiteral(value = value, parameter = parameter).toSuccess()
    }
    
    fun resolve(type: HirType.Union): Result<ThirType, ResolveError>
    {
        val types = type.types.mapUntilError { resolve(it) }.valueOr { return it.toFailure() }
        
        return ThirTypeUnion(types).toSuccess()
    }
    
    private fun resolve(parameter: HirParameterDynamic): Result<Named<ThirType>, ResolveError> =
        resolve(parameter.type).mapValue { parameter.name to it }
}
