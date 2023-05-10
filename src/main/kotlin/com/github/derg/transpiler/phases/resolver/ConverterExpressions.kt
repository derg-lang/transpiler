package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.hir.SymbolTable
import com.github.derg.transpiler.source.hir.Value
import com.github.derg.transpiler.util.Result

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
        is Access.Function       -> ConverterInvoke(symbols)(expression)
        is Access.Subscript      -> TODO()
        is Access.Variable       -> ConverterRead(symbols)(expression)
        is Constant.Bool         -> ConverterBool(expression)
        is Constant.Real         -> ConverterReal(symbols)(expression)
        is Constant.Text         -> ConverterText(symbols)(expression)
        is Operator.Add          -> ConverterAdd(symbols)(expression)
        is Operator.And          -> ConverterAnd(symbols)(expression)
        is Operator.Catch        -> TODO()
        is Operator.Divide       -> ConverterDivide(symbols)(expression)
        is Operator.Equal        -> ConverterEqual(symbols)(expression)
        is Operator.Greater      -> ConverterGreater(symbols)(expression)
        is Operator.GreaterEqual -> ConverterGreaterEqual(symbols)(expression)
        is Operator.Less         -> ConverterLess(symbols)(expression)
        is Operator.LessEqual    -> ConverterLessEqual(symbols)(expression)
        is Operator.Minus        -> ConverterUnaryMinus(symbols)(expression)
        is Operator.Modulo       -> ConverterModulo(symbols)(expression)
        is Operator.Multiply     -> ConverterMultiply(symbols)(expression)
        is Operator.Not          -> ConverterNot(symbols)(expression)
        is Operator.NotEqual     -> ConverterNotEqual(symbols)(expression)
        is Operator.Or           -> ConverterOr(symbols)(expression)
        is Operator.Plus         -> ConverterUnaryPlus(symbols)(expression)
        is Operator.Raise        -> TODO()
        is Operator.Subtract     -> ConverterSubtract(symbols)(expression)
        is Operator.ThreeWay     -> TODO()
        is Operator.Xor          -> ConverterXor(symbols)(expression)
        is When                  -> TODO()
    }
}
