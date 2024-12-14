package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * The symbol resolver ensures that all symbols declared within the [scope] are appropriately converted into typed
 * versions. Resolution depends on the [types] which are available in the type table. Note that resolution cannot take
 * place unless all types that are visible from the scope have been resolved and recorded in the type table.
 *
 * All symbols resolution takes place before the symbol table has been constructed. During symbol resolution, all
 * symbols will be constructed and recorded in the symbol table. After the symbol resolution phase has taken place, all
 * references between objects have been resolved, but not necessarily verified to be correct.
 */
internal class ResolverSymbol(private val symbols: SymbolTable, private val types: TypeTable, private val scope: Scope)
{
    /**
     * Converts the symbol indicated by the [node] to a typed version. This operation converts all raw names into the
     * actual id of the objects referenced where possible. If type conversion fails, an error describing the cause of
     * failure will be returned.
     */
    fun resolve(node: HirSymbol): Result<ThirSymbol, ResolveError> = when (node)
    {
        is HirConcept   -> TODO()
        is HirConstant  -> TODO()
        is HirField     -> handle(node)
        is HirFunction  -> handle(node)
        is HirGeneric   -> TODO()
        is HirLiteral   -> handle(node)
        is HirMethod    -> TODO()
        is HirParameter -> handle(node)
        is HirStruct    -> handle(node)
        is HirVariable  -> handle(node)
    }
    
    private fun handle(node: HirField): Result<ThirField, ResolveError>
    {
        val symbol = ThirField(
            id = node.id,
            name = node.name,
            type = types.fields[node.id]!!,
            value = node.value?.let { ResolverValue(types, scope).resolve(it) }?.valueOr { return it.toFailure() },
            visibility = node.visibility,
            assignability = node.assignability,
        )
        
        symbols.fields[symbol.id] = symbol
        return symbol.toSuccess()
    }
    
    private fun handle(node: HirFunction): Result<ThirSymbol, ResolveError>
    {
        val inner = Scope(scope)
    
//        node.generics.forEach { inner.register(it) }
        node.variables.forEach { inner.register(it) }
        node.parameters.forEach { inner.register(it) }
//        node.generics.mapUntilError { handle(it) }.onFailure { return it.toFailure() }
        node.variables.mapUntilError { handle(it) }.onFailure { return it.toFailure() }
        node.parameters.mapUntilError { handle(it) }.onFailure { return it.toFailure() }
        
        val symbol = ThirFunction(
            id = node.id,
            name = node.name,
            type = types.functions[node.id]!!,
            visibility = node.visibility,
            instructions = node.instructions.mapUntilError { ResolverInstruction(types, inner).resolve(it) }.valueOr { return it.toFailure() },
            genericIds = node.generics.map { it.id },
            variableIds = node.variables.map { it.id },
            parameterIds = node.parameters.map { it.id },
        )
        
        symbols.functions[symbol.id] = symbol
        return symbol.toSuccess()
    }
    
    private fun handle(node: HirLiteral): Result<ThirSymbol, ResolveError>
    {
        val inner = Scope(scope)

        node.variables.forEach { inner.register(it) }
        inner.register(node.parameter)
        node.variables.mapUntilError { handle(it) }.onFailure { return it.toFailure() }
        handle(node.parameter).onFailure { return it.toFailure() }
        
        val symbol = ThirFunction(
            id = node.id,
            name = node.name,
            type = types.literals[node.id]!!,
            visibility = node.visibility,
            instructions = node.instructions.mapUntilError { ResolverInstruction(types, inner).resolve(it) }.valueOr { return it.toFailure() },
            genericIds = emptyList(),
            variableIds = node.variables.map { it.id },
            parameterIds = listOf(node.parameter.id),
        )
        
        symbols.functions[symbol.id] = symbol
        return symbol.toSuccess()
    }
    
    private fun handle(node: HirParameter): Result<ThirSymbol, ResolveError>
    {
        val symbol = ThirParameter(
            id = node.id,
            name = node.name,
            type = types.parameters[node.id]!!,
            value = node.value?.let { ResolverValue(types, scope).resolve(it) }?.valueOr { return it.toFailure() },
            passability = node.passability,
        )
        
        symbols.parameters[symbol.id] = symbol
        return symbol.toSuccess()
    }
    
    private fun handle(node: HirStruct): Result<ThirSymbol, ResolveError>
    {
        node.fields.mapUntilError { handle(it) }.onFailure { return it.toFailure() }
//        node.methods.mapUntilError { handle(it) }.onFailure { return it.toFailure() }
//        node.generics.mapUntilError { handle(it) }.onFailure { return it.toFailure() }
        
        val symbol = ThirStruct(
            id = node.id,
            name = node.name,
            visibility = node.visibility,
            fieldIds = node.fields.map { it.id }.toSet(),
            methodIds = node.methods.map { it.id }.toSet(),
            genericIds = node.generics.map { it.id }.toSet(),
        )
        
        symbols.structs[symbol.id] = symbol
        return symbol.toSuccess()
    }
    
    private fun handle(node: HirVariable): Result<ThirSymbol, ResolveError>
    {
        val symbol = ThirVariable(
            id = node.id,
            name = node.name,
            type = types.variables[node.id]!!,
            assignability = node.assignability,
        )
        
        symbols.variables[symbol.id] = symbol
        return symbol.toSuccess()
    }
}
