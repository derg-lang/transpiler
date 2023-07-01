package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.util.*

/**
 * Generates a value from an invocation of the [function], using the provided values as [parameters].
 */
private fun valueOf(function: ThirFunction, parameters: List<ThirValue>): ThirValue = when (function.value.id)
{
    Builtin.BOOL.id  -> BoolCall(function, parameters)
    Builtin.INT32.id -> Int32Call(function, parameters)
    Builtin.INT64.id -> Int64Call(function, parameters)
    else             -> UserDefinedCall(function, parameters)
}

/**
 * Generates a value from a memory read of the [variable].
 */
private fun valueOf(variable: ThirVariable): ThirValue = when (variable.type.id)
{
    Builtin.BOOL.id  -> BoolRead(variable)
    Builtin.INT32.id -> Int32Read(variable)
    Builtin.INT64.id -> Int64Read(variable)
    else             -> UserDefinedRead(variable)
}

internal class ConverterAnd(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstAnd): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueBool && rhs is ThirValueBool)
            return BoolAnd(lhs, rhs).toSuccess()
        
        return ResolveError.MismatchedFunctionTypes(SymbolType.AND.symbol, listOf(lhs.type, rhs.type)).toFailure()
    }
}

internal class ConverterAdd(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstAdd): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Add(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Add(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.PLUS.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal object ConverterBool
{
    operator fun invoke(node: AstBool): Result<ThirValue, ResolveError> =
        BoolConst(node.value).toSuccess()
}

internal class ConverterCall(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstCall): Result<ThirValue, ResolveError>
    {
        // TODO: Determine parameter ordering based on names as well - just position alone is not enough
        val values = node.arguments
            .fold { symbols.resolveValue(it.expression) }
            .valueOr { return failureOf(it) }
        
        val function = symbols
            .resolveFunction(node.name, values.map { it.type })
            .valueOr { return failureOf(it) }
        return valueOf(function, values).toSuccess()
    }
}

internal class ConverterDivide(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstDivide): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Div(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Div(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.DIVIDE.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterEqual(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstEqual): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueBool && rhs is ThirValueBool)
            return BoolEq(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Eq(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Eq(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.EQUAL.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterGreater(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstGreater): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Gt(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Gt(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.GREATER.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterGreaterEqual(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstGreaterEqual): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Ge(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Ge(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.GREATER_EQUAL.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterLess(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstLess): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Lt(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Lt(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.LESS.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterLessEqual(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstLessEqual): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Le(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Le(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.LESS_EQUAL.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterModulo(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstModulo): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Mod(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Mod(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.MODULO.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterMultiply(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstMultiply): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Mul(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Mul(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.MULTIPLY.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterNot(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstNot): Result<ThirValue, ResolveError>
    {
        val expr = symbols.resolveValue(node.expression).valueOr { return failureOf(it) }
        
        if (expr is ThirValueBool)
            return BoolNot(expr).toSuccess()
        
        return ResolveError.MismatchedFunctionTypes(SymbolType.NOT.symbol, listOf(expr.type)).toFailure()
    }
}

internal class ConverterNotEqual(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstNotEqual): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueBool && rhs is ThirValueBool)
            return BoolNe(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Ne(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Ne(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.NOT_EQUAL.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterOr(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstOr): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueBool && rhs is ThirValueBool)
            return BoolOr(lhs, rhs).toSuccess()
        
        return ResolveError.MismatchedFunctionTypes(SymbolType.OR.symbol, listOf(lhs.type, rhs.type)).toFailure()
    }
}

internal class ConverterRead(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstRead): Result<ThirValue, ResolveError>
    {
        val variable = symbols.resolveVariable(node.name).valueOr { return failureOf(it) }
        return valueOf(variable).toSuccess()
    }
}

internal class ConverterReal(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstReal): Result<ThirValue, ResolveError>
    {
        // TODO: Fail operation if number is too large
        if (node.literal == Builtin.LIT_INT32 || node.literal == null)
            return Int32Const(node.value.toInt()).toSuccess()
        if (node.literal == Builtin.LIT_INT64)
            return Int64Const(node.value.toLong()).toSuccess()
        
        // TODO: Support custom literals
        return ResolveError.UnknownLiteral(node.literal).toFailure()
    }
}

internal class ConverterSubtract(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstSubtract): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueInt32 && rhs is ThirValueInt32)
            return Int32Sub(lhs, rhs).toSuccess()
        if (lhs is ThirValueInt64 && rhs is ThirValueInt64)
            return Int64Sub(lhs, rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.MINUS.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterText(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstText): Result<ThirValue, ResolveError>
    {
        // TODO: Implement me
        if (node.literal == null)
            return ThirValueStrUnicode.toSuccess()
        
        // TODO: Support custom literals
        return ResolveError.UnknownLiteral(node.literal).toFailure()
    }
}

internal class ConverterUnaryMinus(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstMinus): Result<ThirValue, ResolveError>
    {
        val rhs = symbols.resolveValue(node.expression).valueOr { return failureOf(it) }
        
        if (rhs is ThirValueInt32)
            return Int32Neg(rhs).toSuccess()
        if (rhs is ThirValueInt64)
            return Int64Neg(rhs).toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.MINUS.symbol, listOf(rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(rhs)).toSuccess()
    }
}

internal class ConverterUnaryPlus(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstPlus): Result<ThirValue, ResolveError>
    {
        val rhs = symbols.resolveValue(node.expression).valueOr { return failureOf(it) }
        
        if (rhs is ThirValueInt32)
            return rhs.toSuccess()
        if (rhs is ThirValueInt64)
            return rhs.toSuccess()
        
        val function = symbols.resolveFunction(SymbolType.PLUS.symbol, listOf(rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(rhs)).toSuccess()
    }
}

internal class ConverterXor(private val symbols: ThirSymbolTable)
{
    operator fun invoke(node: AstXor): Result<ThirValue, ResolveError>
    {
        val lhs = symbols.resolveValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ThirValueBool && rhs is ThirValueBool)
            return BoolXor(lhs, rhs).toSuccess()
        
        return ResolveError.MismatchedFunctionTypes(SymbolType.XOR.symbol, listOf(lhs.type, rhs.type)).toFailure()
    }
}
