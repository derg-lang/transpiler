package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Access
import com.github.derg.transpiler.source.ast.Constant
import com.github.derg.transpiler.source.ast.Operator
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.hir.Function
import com.github.derg.transpiler.source.lexeme.SymbolType
import com.github.derg.transpiler.util.*

/**
 * Generates a value from an invocation of the [function], using the provided values as [parameters].
 */
private fun valueOf(function: Function, parameters: List<Value>): Value = when (function.value.id)
{
    Builtin.BOOL.id  -> BoolCall(function, parameters)
    Builtin.INT32.id -> Int32Call(function, parameters)
    Builtin.INT64.id -> Int64Call(function, parameters)
    else             -> UserDefinedCall(function, parameters)
}

/**
 * Generates a value from a memory read of the [variable].
 */
private fun valueOf(variable: Variable): Value = when (variable.type.id)
{
    Builtin.BOOL.id  -> BoolRead(variable)
    Builtin.INT32.id -> Int32Read(variable)
    Builtin.INT64.id -> Int64Read(variable)
    else             -> UserDefinedRead(variable)
}

internal class ConverterAnd(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.And): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueBool && rhs is ValueBool)
            return BoolAnd(lhs, rhs).toSuccess()
        
        return ResolveError.MismatchedFunctionTypes(SymbolType.AND.symbol, listOf(lhs.type, rhs.type)).toFailure()
    }
}

internal class ConverterAdd(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Add): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Add(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Add(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.PLUS.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal object ConverterBool
{
    operator fun invoke(node: Constant.Bool): Result<Value, ResolveError> =
        BoolConst(node.value).toSuccess()
}

internal class ConverterDivide(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Divide): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Div(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Div(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.DIVIDE.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterEqual(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Equal): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueBool && rhs is ValueBool)
            return BoolEq(lhs, rhs).toSuccess()
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Eq(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Eq(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.EQUAL.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterGreater(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Greater): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Gt(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Gt(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.GREATER.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterGreaterEqual(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.GreaterEqual): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Ge(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Ge(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.GREATER_EQUAL.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterInvoke(private val symbols: SymbolTable)
{
    operator fun invoke(node: Access.Function): Result<Value, ResolveError>
    {
        // TODO: Determine parameter ordering based on names as well - just position alone is not enough
        val values = node.arguments
            .fold { symbols.resolveRequiredValue(it.expression) }
            .valueOr { return failureOf(it) }
        
        val function = symbols
            .resolveRequiredFunction(node.name, values.map { it.type })
            .valueOr { return failureOf(it) }
        return valueOf(function, values).toSuccess()
    }
}

internal class ConverterLess(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Less): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Lt(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Lt(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.LESS.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterLessEqual(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.LessEqual): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Le(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Le(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.LESS_EQUAL.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterModulo(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Modulo): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Mod(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Mod(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.MODULO.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterMultiply(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Multiply): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Mul(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Mul(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.MULTIPLY.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterNot(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Not): Result<Value, ResolveError>
    {
        val expr = symbols.resolveRequiredValue(node.expression).valueOr { return failureOf(it) }
        
        if (expr is ValueBool)
            return BoolNot(expr).toSuccess()
        
        return ResolveError.MismatchedFunctionTypes(SymbolType.NOT.symbol, listOf(expr.type)).toFailure()
    }
}

internal class ConverterNotEqual(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.NotEqual): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueBool && rhs is ValueBool)
            return BoolNe(lhs, rhs).toSuccess()
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Ne(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Ne(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.NOT_EQUAL.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterOr(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Or): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueBool && rhs is ValueBool)
            return BoolOr(lhs, rhs).toSuccess()
        
        return ResolveError.MismatchedFunctionTypes(SymbolType.OR.symbol, listOf(lhs.type, rhs.type)).toFailure()
    }
}

internal class ConverterRead(private val symbols: SymbolTable)
{
    operator fun invoke(node: Access.Variable): Result<Value, ResolveError>
    {
        val variable = symbols.resolveRequiredVariable(node.name).valueOr { return failureOf(it) }
        return valueOf(variable).toSuccess()
    }
}

internal class ConverterReal(private val symbols: SymbolTable)
{
    operator fun invoke(node: Constant.Real): Result<Value, ResolveError>
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

internal class ConverterSubtract(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Subtract): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueInt32 && rhs is ValueInt32)
            return Int32Sub(lhs, rhs).toSuccess()
        if (lhs is ValueInt64 && rhs is ValueInt64)
            return Int64Sub(lhs, rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.MINUS.symbol, listOf(lhs.type, rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(lhs, rhs)).toSuccess()
    }
}

internal class ConverterText(private val symbols: SymbolTable)
{
    operator fun invoke(node: Constant.Text): Result<Value, ResolveError>
    {
        // TODO: Implement me
        if (node.literal == null)
            return ValueStrUnicode.toSuccess()
        
        // TODO: Support custom literals
        return ResolveError.UnknownLiteral(node.literal).toFailure()
    }
}

internal class ConverterUnaryMinus(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Minus): Result<Value, ResolveError>
    {
        val rhs = symbols.resolveRequiredValue(node.expression).valueOr { return failureOf(it) }
        
        if (rhs is ValueInt32)
            return Int32Neg(rhs).toSuccess()
        if (rhs is ValueInt64)
            return Int64Neg(rhs).toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.MINUS.symbol, listOf(rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(rhs)).toSuccess()
    }
}

internal class ConverterUnaryPlus(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Plus): Result<Value, ResolveError>
    {
        val rhs = symbols.resolveRequiredValue(node.expression).valueOr { return failureOf(it) }
        
        if (rhs is ValueInt32)
            return rhs.toSuccess()
        if (rhs is ValueInt64)
            return rhs.toSuccess()
        
        val function = symbols.resolveRequiredFunction(SymbolType.PLUS.symbol, listOf(rhs.type))
            .valueOr { return failureOf(it) }
        return valueOf(function, listOf(rhs)).toSuccess()
    }
}

internal class ConverterXor(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Xor): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueBool && rhs is ValueBool)
            return BoolXor(lhs, rhs).toSuccess()
        
        return ResolveError.MismatchedFunctionTypes(SymbolType.XOR.symbol, listOf(lhs.type, rhs.type)).toFailure()
    }
}
