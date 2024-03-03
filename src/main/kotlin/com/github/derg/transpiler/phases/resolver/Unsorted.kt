package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * Retrieves a type from the [symbols] table, matching the given [name]. If multiple symbols are accessible under the
 * same name, only the symbol declared last in source will be retrieved - the returned symbol is said to shadow all
 * other symbols.
 */
internal fun resolveType(symbols: ThirSymbolTable, name: String): Result<ThirType, ResolveError>
{
    val candidates = symbols[name].filterIsInstance<ThirType>()
    if (candidates.isEmpty())
        return ResolveError.UnknownType(name).toFailure()
    return candidates[0].toSuccess()
}

/**
 * Retrieves a variable from the [symbols] table, matching the given [name]. If multiple symbols are accessible
 * under the same name, only the symbol declared last in source will be retrieved - the returned symbol is said to
 * shadow all other symbols.
 */
internal fun resolveVariable(symbols: ThirSymbolTable, name: String): Result<ThirVariable, ResolveError>
{
    val candidates = symbols[name].filterIsInstance<ThirVariable>()
    if (candidates.isEmpty())
        return ResolveError.UnknownVariable(name).toFailure()
    return candidates[0].toSuccess()
}

/**
 * Converts all [statements] from ast to thir, registering the new symbols in the provided [scope]. All instructions
 * which were resolved are added to the [scope]'s instruction set.
 */
internal fun resolveScope(scope: ThirScope, statements: List<AstInstruction>): Result<Unit, ResolveError>
{
    val definitionConverter = ConverterDefinitions(scope.symbols)
    val statementConverter = ConverterStatements(scope.symbols)
    
    for (statement in statements)
    {
        if (statement is AstVariable)
            definitionConverter(listOf(statement)).onFailure { return it.toFailure() }
        val instruction = statementConverter(statement).valueOr { return it.toFailure() }
        
        scope.instructions.add(instruction)
    }
    return Unit.toSuccess()
}
