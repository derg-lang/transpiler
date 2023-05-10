package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Operator
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.lexeme.SymbolType
import com.github.derg.transpiler.util.*

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
