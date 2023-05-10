package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.IdProvider
import com.github.derg.transpiler.source.IdProviderSystem
import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.ast.Definition
import com.github.derg.transpiler.source.ast.Expression
import com.github.derg.transpiler.source.ast.Parameter
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.hir.Function
import com.github.derg.transpiler.util.*

/**
 * Converts the given [name] into a type which has it as the name. Any other symbol with the same name, but are not a
 * type, will be ignored. If the name is not provided, [Builtin.VOID] is returned instead.
 */
private fun SymbolTable.resolveOptionalType(name: Name?): Result<Type, ResolveError>
{
    if (name == null)
        return Builtin.VOID.toSuccess()
    
    val symbol = find(name).filterIsInstance<Type>().firstOrNull()
    return symbol?.toSuccess() ?: ResolveError.UnknownType(name).toFailure()
}

/**
 * Converts the given [expression] into a value which represents the same expression. If no expression was provided, an
 * empty value is returned instead.
 */
private fun SymbolTable.resolveOptionalValue(expression: Expression?): Result<Value?, ResolveError>
{
    if (expression == null)
        return successOf(null)
    return ConverterExpressions(this).convert(expression)
}

/**
 * Function declarations contain information which must be type-checked and verified. The function declaration contains
 * all information required to later reference a function in a type-safe manner.
 */
class DeclaratorFunction(private val symbols: SymbolTable, private val ids: IdProvider = IdProviderSystem)
{
    operator fun invoke(node: Definition.Function): Result<Function, ResolveError>
    {
        val declarator = DeclaratorParameter(symbols, ids)
        
        val valueType = symbols.resolveOptionalType(node.valueType).valueOr { return failureOf(it) }
        val errorType = symbols.resolveOptionalType(node.errorType).valueOr { return failureOf(it) }
        val parameters = node.parameters.map { p -> declarator(p).valueOr { return failureOf(it) } }
        
        return Function(
            id = ids.random(),
            name = node.name,
            visibility = node.visibility,
            value = valueType,
            error = errorType,
            params = parameters,
        ).also { it.symbols = SymbolTable(symbols) }.also { symbols.register(it) }.toSuccess()
    }
}

/**
 * Parameter declarations contain information about a value which is passed into a function; this information must be
 * type-checked and sanitized, to verify that the optional value resolves to the same type as the parameter.
 */
class DeclaratorParameter(private val symbols: SymbolTable, private val ids: IdProvider = IdProviderSystem)
{
    operator fun invoke(node: Parameter): Result<Function.Parameter, ResolveError>
    {
        val type = symbols.resolveOptionalType(node.type).valueOr { return failureOf(it) }
        val value = symbols.resolveOptionalValue(node.value).valueOr { return failureOf(it) }
        
        if (value != null && value.type.id != type.id)
            return ResolveError.MismatchedParameterType(expected = type, actual = value.type).toFailure()
        
        return Function.Parameter(
            id = ids.random(),
            name = node.name,
            type = type,
            passability = node.passability,
            value = value,
        ).also { symbols.register(it) }.toSuccess()
    }
}
