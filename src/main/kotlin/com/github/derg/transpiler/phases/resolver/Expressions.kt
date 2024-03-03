package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * The expression converter works on a whole expression at a time, converting it into a single value representing the
 * initial expression. All conversion takes place at the scope defined by the [symbols] table.
 */
internal class ConverterExpression(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstExpression): Result<ThirValue, ResolveError> = when (node)
    {
        is AstAdd          -> resolve(Symbol.PLUS, node.lhs, node.rhs)
        is AstAnd          -> resolve(Symbol.AND, node.lhs, node.rhs)
        is AstDivide       -> resolve(Symbol.DIVIDE, node.lhs, node.rhs)
        is AstEqual        -> resolve(Symbol.EQUAL, node.lhs, node.rhs)
        is AstGreater      -> resolve(Symbol.GREATER, node.lhs, node.rhs)
        is AstGreaterEqual -> resolve(Symbol.GREATER_EQUAL, node.lhs, node.rhs)
        is AstLess         -> resolve(Symbol.LESS, node.lhs, node.rhs)
        is AstLessEqual    -> resolve(Symbol.LESS_EQUAL, node.lhs, node.rhs)
        is AstMinus        -> resolve(Symbol.MINUS, node.expression)
        is AstModulo       -> resolve(Symbol.MODULO, node.lhs, node.rhs)
        is AstMultiply     -> resolve(Symbol.MULTIPLY, node.lhs, node.rhs)
        is AstNot          -> resolve(Symbol.NOT, node.expression)
        is AstNotEqual     -> resolve(Symbol.NOT_EQUAL, node.lhs, node.rhs)
        is AstOr           -> resolve(Symbol.OR, node.lhs, node.rhs)
        is AstPlus         -> resolve(Symbol.PLUS, node.expression)
        is AstSubtract     -> resolve(Symbol.MINUS, node.lhs, node.rhs)
        is AstXor          -> resolve(Symbol.XOR, node.lhs, node.rhs)
        is AstCall         -> resolveFun(node.name, node.valArgs)
        is AstRead         -> resolveVar(node.name)
        is AstBool         -> parse(node.value)
        is AstReal         -> parse(node.value, node.literal)
        is AstText         -> parse(node.value, node.literal)
        is AstCatch        -> TODO()
        is AstRaise        -> TODO()
        is AstThreeWay     -> TODO()
        is AstWhen         -> TODO()
    }
    
    /**
     * Resolves a function for the given [operator], which accepts the provided [arguments]. The arguments must be in
     * the same order as they are expected by the operator function.
     */
    private fun resolve(operator: Symbol, vararg arguments: AstExpression): Result<ThirValue, ResolveError> =
        resolveFun(operator.symbol, arguments.map { AstArgument(null, it) })
    
    /**
     * Resolves a variable read for the memory access bound by [name]. The read may be against a variable, parameter,
     * property, or similar named locations in memory.
     */
    private fun resolveVar(name: String): Result<ThirValue, ResolveError>
    {
        val candidates = symbols[name].filter { it is ThirVariable || it is ThirParameter }
        val candidate = candidates.firstOrNull() ?: return ResolveError.UnknownVariable(name).toFailure()
        
        val type = when (candidate)
        {
            is ThirVariable  -> candidate.type
            is ThirParameter -> candidate.type
            else             -> Builtin.VOID.id
        }
        return ThirVariableRead(type, candidate.id).toSuccess()
    }
    
    /**
     * Resolves a function call for the function with the given [name], given the input [arguments]. The arguments may
     * be in any order if they are named.
     */
    private fun resolveFun(name: String, arguments: List<AstArgument>): Result<ThirValue, ResolveError>
    {
        val values = arguments.fold { convert(it) }.valueOr { return it.toFailure() }
        
        // Deny unnamed arguments appearing after named arguments
        val firstNamed = arguments.withIndex().firstOrNull { it.value.name != null }
        val lastUnnamed = arguments.withIndex().lastOrNull { it.value.name == null }
        if (firstNamed != null && lastUnnamed != null && lastUnnamed.index > firstNamed.index)
            return ResolveError.ArgumentMisnamed(name, values).toFailure()
        
        // May now figure out which functions are valid candidates
        val functions = symbols[name].filterIsInstance<ThirFunction>()
        if (functions.isEmpty())
            return ResolveError.UnknownFunction(name).toFailure()
        
        val (candidates, _) = functions.map { convert(it, values) }.partition()
        return when (candidates.size)
        {
            1    -> candidates[0].toBuiltin().toSuccess()
            0    -> ResolveError.ArgumentMismatch(name, values).toFailure()
            else -> ResolveError.ArgumentAmbiguous(name, values).toFailure()
        }
    }
    
    private fun convert(argument: AstArgument): Result<ThirArgument, ResolveError> =
        ThirArgument(argument.name, invoke(argument.expression).valueOr { return it.toFailure() }).toSuccess()
    
    private fun convert(function: ThirFunction, arguments: List<ThirArgument>): Result<ThirFunctionCall, ThirFunction>
    {
        // TODO: Support variadic arguments
        if (function.params.size != arguments.size)
            return function.toFailure()
        
        // Reject function if parameter types do not match function signature
        val nameToIndex = function.params.withIndex().associate { it.value.name to it.index }
        val ordered = arguments.withIndex().sortedBy { nameToIndex[it.value.name] ?: it.index }
        if (function.params.zip(ordered).any { it.first.type != it.second.value.value.valType })
            return function.toFailure()
        
        return ThirFunctionCall(function.valType, function.errType, function.id, ordered.map { it.value }).toSuccess()
    }
    
    /**
     * Extracts a boolean value from the given [value].
     */
    private fun parse(value: Boolean): Result<ThirValue, ResolveError> = ThirBoolConst(value).toSuccess()
    
    /**
     * Extracts a numeric value from the [value], applying the [literal].
     */
    private fun parse(value: Number, literal: String): Result<ThirValue, ResolveError>
    {
        // TODO: Fail operation if number is too large
        if (literal == Builtin.INT32_LIT.name)
            return ThirInt32Const(value.toInt()).toSuccess()
        if (literal == Builtin.INT64_LIT.name)
            return ThirInt64Const(value.toLong()).toSuccess()
        
        // TODO: Support custom literals
        return ResolveError.UnknownLiteral(literal).toFailure()
    }
    
    /**
     * Extracts a text value from the [value], applying the [literal].
     */
    private fun parse(value: String, literal: String): Result<ThirValue, ResolveError>
    {
        // TODO: Fail operation if string is invalid
        if (literal == Builtin.STR_LIT.name)
            TODO("not implemented")
        
        // TODO: Support custom literals
        return ResolveError.UnknownLiteral(literal).toFailure()
    }
}
