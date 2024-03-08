package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import java.util.*

// Literals

val Any.hir: HirValue
    get() = when (this)
    {
        is Boolean -> HirBool(this)
        is Int     -> HirInteger(toBigInteger(), INT32_LIT_NAME)
        is Long    -> HirInteger(toBigInteger(), INT64_LIT_NAME)
        is String  -> HirText(this, STR_LIT_NAME)
        else       -> throw IllegalArgumentException("Value $this does not represent a valid hir value")
    }

// Expressions

infix fun Any.hirEq(that: Any): HirValue = HirEq(this.hir, that.hir)
infix fun Any.hirNe(that: Any): HirValue = HirNe(this.hir, that.hir)
infix fun Any.hirGe(that: Any): HirValue = HirGe(this.hir, that.hir)
infix fun Any.hirGt(that: Any): HirValue = HirGt(this.hir, that.hir)
infix fun Any.hirLe(that: Any): HirValue = HirLe(this.hir, that.hir)
infix fun Any.hirLt(that: Any): HirValue = HirLt(this.hir, that.hir)

infix fun Any.hirAdd(that: Any): HirValue = HirAdd(this.hir, that.hir)
infix fun Any.hirDiv(that: Any): HirValue = HirDiv(this.hir, that.hir)
infix fun Any.hirMod(that: Any): HirValue = HirMod(this.hir, that.hir)
infix fun Any.hirMul(that: Any): HirValue = HirMul(this.hir, that.hir)
infix fun Any.hirSub(that: Any): HirValue = HirSub(this.hir, that.hir)

infix fun Any.hirAnd(that: Any): HirValue = HirAnd(this.hir, that.hir)
infix fun Any.hirOr(that: Any): HirValue = HirOr(this.hir, that.hir)
infix fun Any.hirXor(that: Any): HirValue = HirXor(this.hir, that.hir)

val Any.hirNot: HirValue get() = HirNot(hir)
val Any.hirMinus: HirValue get() = HirMinus(hir)
val Any.hirPlus: HirValue get() = HirPlus(hir)

val HirSymbol.hirLoad: HirValue get() = HirLoad(name, emptyList())
fun HirFunction.hirCall(vararg parameters: Any) = HirCall(hirLoad, parameters.map { null hirArg it })
infix fun String?.hirArg(that: Any) = HirNamedParameter(this, that.hir)

// Instructions

val HirValue.hirEval get() = HirEvaluate(this)
val Any.hirReturnError get() = HirReturnError(hir)
val Any.hirReturnValue get() = HirReturnValue(hir)

infix fun HirVariable.hirAssign(that: Any) = HirAssign(hirLoad, that.hir)

// Symbols

fun hirFunOf(
    name: String = UUID.randomUUID().toString(),
    value: HirType? = null,
    error: HirType? = null,
    params: List<HirParameter> = emptyList(),
) = HirFunction(
    id = UUID.randomUUID(),
    name = name,
    type = HirTypeFunction(
        value = value,
        error = error,
        parameters = params.map { HirTypedParameter(it.name, it.type) },
    ),
    visibility = Visibility.PRIVATE,
    instructions = emptyList(),
    generics = emptyList(),
    variables = emptyList(),
    parameters = params,
)

fun hirLitOf(
    name: String = UUID.randomUUID().toString(),
    value: HirType = Builtin.INT32_TYPE,
    param: HirParameter = hirParamOf(),
) = HirLiteral(
    id = UUID.randomUUID(),
    name = name,
    type = HirTypeLiteral(value = value, parameter = param.type),
    visibility = Visibility.PRIVATE,
    instructions = emptyList(),
    variables = emptyList(),
    parameter = param,
)

fun hirParamOf(
    name: String = UUID.randomUUID().toString(),
    type: HirType = Builtin.INT32_TYPE,
    value: HirValue? = null,
) = HirParameter(
    id = UUID.randomUUID(),
    name = name,
    type = type,
    value = value,
    passability = Passability.IN,
)

fun hirVarOf(
    name: String = UUID.randomUUID().toString(),
    type: HirType? = null,
    value: Any = 0,
) = HirVariable(
    id = UUID.randomUUID(),
    name = name,
    type = type,
    value = value.hir,
    assignability = Assignability.FINAL,
)
