package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.hir.Function
import com.github.derg.transpiler.util.*

/**
 * In order to acquire meaningful values from the source code, in a type-safe manner, all expressions must be converted
 * into type-checked values. Values represent raw computable values at a high level while also preserving a level of
 * control of the machine code.
 *
 * @property symbols The symbol table of the scope the expressions are residing in.
 */
class ConverterExpressions(private val symbols: SymbolTable)
{
    /**
     * Converts the [expression] into a value representing it. Any complicated expression may be provided, allowing the
     * entire chain of operations to be converted in one go.
     */
    fun convert(expression: Expression): Result<Value, ResolveError> = when (expression)
    {
        is Access.Function       -> expression.toValue()
        is Access.Subscript      -> TODO()
        is Access.Variable       -> expression.toValue()
        is Constant.Bool         -> expression.toValue()
        is Constant.Real         -> expression.toValue()
        is Constant.Text         -> TODO()
        is Operator.Add          -> ConverterAdd(symbols)(expression)
        is Operator.And          -> ConverterAnd(symbols)(expression)
        is Operator.Catch        -> TODO()
        is Operator.Divide       -> ConverterDivide(symbols)(expression)
        is Operator.Equal        -> ConverterEqual(symbols)(expression)
        is Operator.Greater      -> ConverterGreater(symbols)(expression)
        is Operator.GreaterEqual -> ConverterGreaterEqual(symbols)(expression)
        is Operator.Less         -> ConverterLess(symbols)(expression)
        is Operator.LessEqual    -> ConverterLessEqual(symbols)(expression)
        is Operator.Minus        -> expression.toValue()
        is Operator.Modulo       -> ConverterModulo(symbols)(expression)
        is Operator.Multiply     -> ConverterMultiply(symbols)(expression)
        is Operator.Not          -> ConverterNot(symbols)(expression)
        is Operator.NotEqual     -> ConverterNotEqual(symbols)(expression)
        is Operator.Or           -> ConverterOr(symbols)(expression)
        is Operator.Plus         -> expression.toValue()
        is Operator.Raise        -> TODO()
        is Operator.Subtract     -> ConverterSubtract(symbols)(expression)
        is Operator.ThreeWay     -> TODO()
        is Operator.Xor          -> ConverterXor(symbols)(expression)
        is When                  -> TODO()
    }
    
    private fun Access.Function.toValue(): Result<Value, ResolveError>
    {
        val params = arguments.fold { convert(it.expression) }.valueOr { return failureOf(it) }
        for (symbol in symbols.find(name))
        {
            // TODO: Support variable invoking
            if (symbol is Variable)
                return ResolveError.Unsupported.toFailure()
            
            if (symbol is Function)
            {
                // TODO: Ensure all parameters are sorted in correct order
                // TODO: Verify that we actually found the correct overload
                return when (symbol.value.id)
                {
                    Builtin.BOOL.id  -> BoolCall(symbol, params)
                    Builtin.INT32.id -> Int32Call(symbol, params)
                    Builtin.INT64.id -> Int64Call(symbol, params)
                    else             -> UserDefinedCall(symbol, params)
                }.toSuccess()
            }
        }
        return ResolveError.UnknownCallable(name).toFailure()
    }
    
    private fun Access.Variable.toValue(): Result<Value, ResolveError>
    {
        val symbol = symbols.find(name).firstOrNull() ?: return ResolveError.Unknown.toFailure()
        if (symbol is Variable)
        {
            return when (symbol.type.id)
            {
                Builtin.BOOL.id  -> BoolRead(symbol)
                Builtin.INT32.id -> Int32Read(symbol)
                Builtin.INT64.id -> Int64Read(symbol)
                else             -> UserDefinedRead(symbol)
            }.toSuccess()
        }
        
        // TODO: Support reading functions as values, too
        // TODO: Support reading types as values, too...?
        return ResolveError.Unsupported.toFailure()
    }
    
    private fun Constant.Bool.toValue(): Result<Value, ResolveError> =
        BoolConst(value).toSuccess()
    
    private fun Constant.Real.toValue(): Result<Value, ResolveError> = when (literal)
    {
        Builtin.LIT_INT32, null -> Int32Const(value.toInt()).toSuccess() // TODO: Fail operation if number is too large
        Builtin.LIT_INT64       -> Int64Const(value.toLong()).toSuccess() // TODO: Fail operation if number is too large
        else                    -> ResolveError.UnknownLiteral(literal).toFailure()
    }
    
    private fun Operator.Minus.toValue(): Result<Value, ResolveError>
    {
        val value = convert(expression).valueOr { return failureOf(it) }
        return when (value)
        {
            is ValueInt32 -> Int32Neg(value).toSuccess()
            is ValueInt64 -> Int64Neg(value).toSuccess()
            else          -> ResolveError.Unsupported.toFailure()
        }
    }
    
    private fun Operator.Plus.toValue(): Result<Value, ResolveError>
    {
        val value = convert(expression).valueOr { return failureOf(it) }
        return when (value)
        {
            is ValueInt32 -> value.toSuccess()
            is ValueInt64 -> value.toSuccess()
            else          -> ResolveError.Unsupported.toFailure()
        }
    }
}
