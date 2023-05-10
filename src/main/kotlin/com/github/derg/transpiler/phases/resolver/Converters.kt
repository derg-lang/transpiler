package com.github.derg.transpiler.phases.resolver

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

class ConverterAnd(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.And): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueBool && rhs is ValueBool)
            return BoolAnd(lhs, rhs).toSuccess()
        
        return ResolveError.MismatchedCallableParams(SymbolType.AND.symbol, listOf(lhs.type, rhs.type)).toFailure()
    }
}

class ConverterAdd(private val symbols: SymbolTable)
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

class ConverterDivide(private val symbols: SymbolTable)
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

class ConverterModulo(private val symbols: SymbolTable)
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

class ConverterMultiply(private val symbols: SymbolTable)
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

class ConverterNot(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Not): Result<Value, ResolveError>
    {
        val expr = symbols.resolveRequiredValue(node.expression).valueOr { return failureOf(it) }
        
        if (expr is ValueBool)
            return BoolNot(expr).toSuccess()
        
        return ResolveError.MismatchedCallableParams(SymbolType.NOT.symbol, listOf(expr.type)).toFailure()
    }
}

class ConverterOr(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Or): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueBool && rhs is ValueBool)
            return BoolOr(lhs, rhs).toSuccess()
        
        return ResolveError.MismatchedCallableParams(SymbolType.OR.symbol, listOf(lhs.type, rhs.type)).toFailure()
    }
}

class ConverterSubtract(private val symbols: SymbolTable)
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

class ConverterXor(private val symbols: SymbolTable)
{
    operator fun invoke(node: Operator.Xor): Result<Value, ResolveError>
    {
        val lhs = symbols.resolveRequiredValue(node.lhs).valueOr { return failureOf(it) }
        val rhs = symbols.resolveRequiredValue(node.rhs).valueOr { return failureOf(it) }
        
        if (lhs is ValueBool && rhs is ValueBool)
            return BoolXor(lhs, rhs).toSuccess()
        
        return ResolveError.MismatchedCallableParams(SymbolType.XOR.symbol, listOf(lhs.type, rhs.type)).toFailure()
    }
}
