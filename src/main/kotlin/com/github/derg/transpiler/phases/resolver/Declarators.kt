package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.IdProvider
import com.github.derg.transpiler.source.IdProviderSystem
import com.github.derg.transpiler.source.ast.Definition
import com.github.derg.transpiler.source.ast.Parameter
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.hir.Function
import com.github.derg.transpiler.util.*

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
            value = valueType,
            error = errorType,
            params = parameters,
            visibility = node.visibility,
        ).also { symbols.register(it) }.toSuccess()
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
            value = value,
            passability = node.passability,
        ).also { symbols.register(it) }.toSuccess()
    }
}

/**
 * Type declarations contain information about the type itself. The declaration only allows the type to be referred to
 * at a later time, but does not include any information about the contents of the type.
 */
class DeclaratorType(private val symbols: SymbolTable, private val ids: IdProvider = IdProviderSystem)
{
    operator fun invoke(node: Definition.Type): Result<Type, ResolveError>
    {
        return Type(
            id = ids.random(),
            name = node.name,
            visibility = node.visibility,
        ).also { symbols.register(it) }.toSuccess()
    }
}

/**
 * Variable declaration contain information about the value which is stored in memory. This information must be
 * type-checked and sanitized, to verify that the optional type resolves to the same type as the value of the variable.
 */
class DeclaratorVariable(private val symbols: SymbolTable, private val ids: IdProvider = IdProviderSystem)
{
    operator fun invoke(node: Definition.Variable): Result<Variable, ResolveError>
    {
        // Note: Type inference fails here if the order in which variables are declared does not correspond to the
        //       order in which they are initialized.
        val type = symbols.resolveOptionalType(node.type).valueOr { return failureOf(it) }
        val value = symbols.resolveRequiredValue(node.value).valueOr { return failureOf(it) }
        
        if (value.type.id != type.id && type.id != Builtin.VOID.id)
            return ResolveError.MismatchedVariableType(expected = type, actual = value.type).toFailure()
        
        return Variable(
            id = ids.random(),
            name = node.name,
            type = value.type,
            visibility = node.visibility,
            mutability = node.mutability,
            assignability = node.assignability,
        ).also { symbols.register(it) }.toSuccess()
    }
}
