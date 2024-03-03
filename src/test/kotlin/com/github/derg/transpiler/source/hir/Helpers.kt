package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import java.util.*

// Literals

val Any.hir: HirValue
    get() = when (this)
    {
        is Boolean -> HirBool(this)
        is Int     -> HirInteger(toBigInteger(), LIT_NAME_I32)
        is Long    -> HirInteger(toBigInteger(), LIT_NAME_I64)
        is String  -> HirText(this, LIT_NAME_STR)
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
    value: HirStruct? = null,
    error: HirStruct? = null,
    params: List<HirParameter> = emptyList(),
) = HirFunction(
    id = UUID.randomUUID(),
    name = name,
    visibility = Visibility.PRIVATE,
    value = value?.let { HirTypeStruct(it.name, emptyList(), Mutability.IMMUTABLE) },
    error = error?.let { HirTypeStruct(it.name, emptyList(), Mutability.IMMUTABLE) },
    instructions = emptyList(),
    generics = emptyList(),
    variables = emptyList(),
    parameters = params,
)

fun hirLitOf(
    name: String = UUID.randomUUID().toString(),
    value: HirStruct = Builtin.VOID,
    param: HirParameter = hirParamOf(type = Builtin.INT32),
) = HirLiteral(
    id = UUID.randomUUID(),
    name = name,
    visibility = Visibility.PRIVATE,
    value = HirTypeStruct(value.name, emptyList(), Mutability.IMMUTABLE),
    instructions = emptyList(),
    variables = emptyList(),
    parameter = param,
)

fun hirParamOf(
    name: String = UUID.randomUUID().toString(),
    type: HirStruct = Builtin.VOID,
    value: HirValue? = null,
) = HirParameter(
    id = UUID.randomUUID(),
    name = name,
    passability = Passability.IN,
    type = HirTypeStruct(type.name, emptyList(), Mutability.IMMUTABLE),
    value = value,
)

fun hirVarOf(
    name: String = UUID.randomUUID().toString(),
    type: HirStruct? = null,
    value: Any = 0,
) = HirVariable(
    id = UUID.randomUUID(),
    name = name,
    mutability = Mutability.IMMUTABLE,
    assignability = Assignability.CONSTANT,
    type = type?.let { HirTypeStruct(it.name, emptyList(), Mutability.IMMUTABLE) },
    value = value.hir,
)
