package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.source.thir.Function
import com.github.derg.transpiler.util.*

/**
 * The declarator is responsible for making sure all definitions are properly registered within the symbol table before
 * any of the instructions are resolved. Declaring all symbols is the initial phase of the resolver phase.
 */
internal class Declarator(private val symbols: SymbolTable, ids: IdProvider)
{
    private val func = DeclaratorFunction(symbols, ids)
    private val type = DeclaratorType(symbols, ids)
    private val vari = DeclaratorVariable(symbols, ids)
    
    operator fun invoke(nodes: List<AstDefinition>): Result<DeclaredSymbols, ResolveError>
    {
        // Must ensure all functions and types are declared in advance, as they may be referring to each other in a
        // non-trivial manners. At any scope level, all functions and types must be able to "see" each other, even
        // before the later functions and types have been declared in source code. A similar story holds for variables.
        // In order to reference the variable by name at a later stage, when assigning values, the variable must already
        // be registered.
        val context = DeclaredSymbols()
        
        val variables = nodes
            .filterIsInstance<AstVariable>()
            .fold { def -> vari(def).mapValue { it to def } }
            .valueOr { return failureOf(it) }
        val functions = nodes
            .filterIsInstance<AstFunction>()
            .fold { def -> func(def).mapValue { it to def.statements } }
            .valueOr { return failureOf(it) }
        val types = nodes
            .filterIsInstance<AstType>()
            .fold { def -> type(def).mapValue { it to def } }
            .valueOr { return failureOf(it) }
        
        // Every declared symbol, must be registered in the symbol table for later referencing
        variables.onEach { symbols.register(it.first) }
        functions.onEach { symbols.register(it.first) }
        types.onEach { symbols.register(it.first) }
        
        // Functions and types must be tightly related to their definitions.
        // TODO: Consider re-structuring this, it is an unnatural coupling
        functions.forEach { context.functions.add(it) }
        types.forEach { context.types.add(it) }
        return successOf(context)
    }
}

internal class DeclaredSymbols
{
    val functions = mutableListOf<Pair<Function, List<AstStatement>>>()
    val types = mutableListOf<Pair<Type, AstType>>()
}

/**
 * Function declarations contain information which must be type-checked and verified. The function declaration contains
 * all information required to later reference a function in a type-safe manner.
 */
internal class DeclaratorFunction(private val symbols: SymbolTable, private val ids: IdProvider)
{
    private val param = DeclaratorParameter(symbols, ids)
    
    operator fun invoke(node: AstFunction): Result<Function, ResolveError>
    {
        val valueType = symbols.resolveOptionalType(node.valueType).valueOr { return failureOf(it) }
        val errorType = symbols.resolveOptionalType(node.errorType).valueOr { return failureOf(it) }
        val parameters = node.parameters.map { p -> param(p).valueOr { return failureOf(it) } }
        
        // TODO: Verify that there are no conflicting functions in the same scope
        return Function(
            id = ids.random(),
            name = node.name,
            value = valueType,
            error = errorType,
            params = parameters,
            visibility = node.visibility,
        ).toSuccess()
    }
}

/**
 * Parameter declarations contain information about a value which is passed into a function; this information must be
 * type-checked and sanitized, to verify that the optional value resolves to the same type as the parameter.
 */
internal class DeclaratorParameter(private val symbols: SymbolTable, private val ids: IdProvider)
{
    operator fun invoke(node: AstParameter): Result<Function.Parameter, ResolveError>
    {
        val type = symbols.resolveOptionalType(node.type).valueOr { return failureOf(it) }
        val value = symbols.resolveOptionalValue(node.value).valueOr { return failureOf(it) }
        
        if (value != null && value.type.id != type.id)
            return ResolveError.MismatchedParameterType(expected = type, actual = value.type).toFailure()
        
        // TODO: Verify that there are no conflicting parameters in the same scope
        return Function.Parameter(
            id = ids.random(),
            name = node.name,
            type = type,
            value = value,
            passability = node.passability,
        ).toSuccess()
    }
}

/**
 * Type declarations contain information about the type itself. The declaration only allows the type to be referred to
 * at a later time, but does not include any information about the contents of the type.
 */
internal class DeclaratorType(private val symbols: SymbolTable, private val ids: IdProvider)
{
    operator fun invoke(node: AstType): Result<Type, ResolveError>
    {
        // TODO: Verify that there are no conflicting types in the same scope
        return Type(
            id = ids.random(),
            name = node.name,
            visibility = node.visibility,
        ).toSuccess()
    }
}

/**
 * Variable declaration contain information about the value which is stored in memory. This information must be
 * type-checked and sanitized, to verify that the optional type resolves to the same type as the value of the variable.
 */
internal class DeclaratorVariable(private val symbols: SymbolTable, private val ids: IdProvider)
{
    operator fun invoke(node: AstVariable): Result<Variable, ResolveError>
    {
        // Note: Type inference fails here if the order in which variables are declared does not correspond to the
        //       order in which they are initialized.
        val type = symbols.resolveOptionalType(node.type).valueOr { return failureOf(it) }
        val value = symbols.resolveRequiredValue(node.value).valueOr { return failureOf(it) }
        
        if (value.type.id != type.id && type.id != Builtin.VOID.id)
            return ResolveError.MismatchedVariableType(expected = type, actual = value.type).toFailure()
        
        // TODO: Verify that there are no conflicting variables in the same scope
        return Variable(
            id = ids.random(),
            name = node.name,
            type = value.type,
            visibility = node.visibility,
            mutability = node.mutability,
            assignability = node.assignability,
        ).toSuccess()
    }
}
