package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * The definition converter operates on a whole symbol at a time, within the context of a single module.
 */
internal class ConverterDefinitions(private val symbols: ThirSymbolTable)
{
    /**
     * Links a registered symbol to the node which defines the symbol. The node has not yet been defined when it is at
     * first registered in this collection.
     */
    private val definitions = mutableMapOf<ThirId.Static, AstDefinition>()
    
    /**
     * Converts the [nodes] and all other definitions found inside the nodes into a typed high-intermediary
     * representation of the abstract syntax tree.
     */
    operator fun invoke(nodes: List<AstDefinition>): Result<Unit, ResolveError>
    {
        // All symbols must be declared, before any can be defined. This is because symbols may refer to each other, on
        // the same scope level.
        for (node in nodes)
        {
            val symbol = when (node)
            {
                is AstType     -> declare(node)
                is AstFunction -> declare(node)
                is AstVariable -> declare(node)
            }.valueOr { return it.toFailure() }
            
            symbols.register(symbol)
            definitions[symbol.id] = node
        }
        
        // Now that we know all symbols exists, we can start defining them.
        for (symbol in definitions.keys.mapNotNull { symbols[it] })
        {
            when (symbol)
            {
                is ThirFunction -> define(symbol).onFailure { return it.toFailure() }
                is ThirType     -> define(symbol).onFailure { return it.toFailure() }
                is ThirVariable -> define(symbol).onFailure { return it.toFailure() }
                else            -> continue
            }
        }
        return Unit.toSuccess()
    }
    
    private fun declare(node: AstFunction): Result<ThirSymbol, ResolveError>
    {
        // TODO: Verify that there exists no conflicting symbols in the same scope
        val symbol = convert(node)
        symbol.params.onEach(symbol.scope.symbols::register)
        return symbol.toSuccess()
    }
    
    private fun declare(node: AstType): Result<ThirSymbol, ResolveError>
    {
        // TODO: Verify that there exists no conflicting symbols in the same scope
        val symbol = convert(node)
        symbol.properties.onEach(symbol.scope.symbols::register)
        return symbol.toSuccess()
    }
    
    private fun declare(node: AstVariable): Result<ThirSymbol, ResolveError>
    {
        // TODO: Verify that there exists no conflicting symbols in the same scope
        return convert(node).toSuccess()
    }
    
    private fun define(symbol: ThirFunction): Result<Unit, ResolveError>
    {
        val node = definitions[symbol.id] as AstFunction
        val expressions = ConverterExpression(symbol.scope.symbols)
        
        // Resolve all parameter types
        for ((i, param) in node.parameters.withIndex())
        {
            val value = param.value?.let { expressions(it) }?.valueOr { return it.toFailure() }
            val type = resolveType(symbols, param.type).valueOr { return it.toFailure() }
            
            if (type.id != value?.valType && value != null)
                return ResolveError.MismatchedParameterType(type.id, value.valType).toFailure()
            
            symbol.params[i].type.resolve(type.id)
        }
        
        // Resolve the function return value and error types
        val valueType = when (node.valueType)
        {
            null -> Builtin.VOID
            else -> resolveType(symbols, node.valueType).valueOr { return it.toFailure() }
        }
        val errorType = when (node.errorType)
        {
            null -> Builtin.VOID
            else -> resolveType(symbols, node.errorType).valueOr { return it.toFailure() }
        }
        symbol.valType.resolve(valueType.id)
        symbol.errType.resolve(errorType.id)
        
        // Resolve all statements in the function to high-level instructions
        resolveScope(symbol.scope, node.statements).onFailure { return it.toFailure() }
        
        // Verify type integrity of all return statements within the function body
        for (instruction in symbol.scope.instructions)
        {
            // TODO: Ensure that the return instruction does not have a non-void error value.
            if (instruction is ThirReturnValue && instruction.value.valType != valueType.id)
                return ResolveError.MismatchedReturnType(valueType.id, instruction.value.valType).toFailure()
            if (instruction is ThirReturnError && instruction.error.valType != errorType.id)
                return ResolveError.MismatchedReturnType(errorType.id, instruction.error.valType).toFailure()
        }
        return Unit.toSuccess()
    }
    
    private fun define(symbol: ThirType): Result<Unit, ResolveError>
    {
        val node = definitions[symbol.id] as AstType
        val expressions = ConverterExpression(symbol.scope.symbols)
    
        // Resolve all property types
        for ((i, prop) in node.properties.withIndex())
        {
            val value = prop.value?.let { expressions(it) }?.valueOr { return it.toFailure() }
            val type = resolveType(symbols, prop.type).valueOr { return it.toFailure() }
        
            if (type.id != value?.valType && value != null)
                return ResolveError.MismatchedParameterType(type.id, value.valType).toFailure()
        
            symbol.properties[i].type.resolve(type.id)
        }
        
        return Unit.toSuccess()
    }
    
    private fun define(symbol: ThirVariable): Result<Unit, ResolveError>
    {
        val node = definitions[symbol.id] as AstVariable
        val converter = ConverterExpression(symbols)
        
        // Resolve the variable type and ensure the value type matches the defined type of the variable, if any.
        val value = converter(node.value).valueOr { return it.toFailure() }
        val type = node.type?.let { resolveType(symbols, it) }?.valueOr { return it.toFailure() }
        
        if (value.valType != type?.id && type != null)
            return ResolveError.MismatchedVariableType(type.id, value.valType).toFailure()
        if (value.errType != Builtin.VOID.id)
            return ResolveError.VariableWithError(value).toFailure()
        
        symbol.type.resolve(value.valType)
        return Unit.toSuccess()
    }
    
    private fun convert(node: AstFunction) = ThirFunction(
        id = ThirId.Static(),
        name = node.name,
        valType = ThirId.Resolvable(),
        errType = ThirId.Resolvable(),
        params = node.parameters.map { convert(it) },
        visibility = node.visibility,
        scope = ThirScope(symbols),
    )
    
    private fun convert(node: AstParameter) = ThirParameter(
        id = ThirId.Static(),
        name = node.name,
        type = ThirId.Resolvable(),
        passability = node.passability,
        defaultValue = null,
    )
    
    private fun convert(node: AstType) = ThirType(
        id = ThirId.Static(),
        name = node.name,
        visibility = node.visibility,
        properties = node.properties.map { convert(it) },
        scope = ThirScope(symbols),
    )
    
    private fun convert(node: AstProperty) = ThirProperty(
        id = ThirId.Static(),
        name = node.name,
        type = ThirId.Resolvable(),
        visibility = node.visibility,
        mutability = node.mutability,
        assignability = node.assignability,
    )
    
    private fun convert(node: AstVariable) = ThirVariable(
        id = ThirId.Static(),
        name = node.name,
        type = ThirId.Resolvable(),
        visibility = node.visibility,
        mutability = node.mutability,
        assignability = node.assignability,
    )
}
