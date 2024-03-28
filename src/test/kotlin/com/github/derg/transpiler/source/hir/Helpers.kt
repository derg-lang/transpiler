package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.utils.*
import java.util.*

/////////////////////
// Literal helpers //
/////////////////////

val Any.hir: HirValue
    get() = when (this)
    {
        is HirValue -> this
        is Boolean  -> HirBool(this)
        is Int      -> HirInteger(toBigInteger(), INT32_LIT_NAME)
        is Long     -> HirInteger(toBigInteger(), INT64_LIT_NAME)
        is String   -> HirText(this, STR_LIT_NAME)
        else        -> throw IllegalArgumentException("Value $this does not represent a valid hir value")
    }

//////////////////
// Type helpers //
//////////////////

fun hirTypeData(
    struct: HirStruct = Builtin.INT32,
    mutability: Mutability = Mutability.IMMUTABLE,
) = HirTypeData(
    name = struct.name,
    generics = emptyList(),
    mutability = mutability,
)

fun hirTypeData(
    name: String = UUID.randomUUID().toString(),
    mutability: Mutability = Mutability.IMMUTABLE,
) = HirTypeData(
    name = name,
    generics = emptyList(),
    mutability = mutability,
)

fun hirTypeCall(
    value: HirType? = null,
    error: HirType? = null,
    parameters: List<HirType> = emptyList(),
) = HirTypeCall(
    value = value,
    error = error,
    parameters = parameters.map { "" to it },
)

fun hirTypeUnion(vararg types: HirType) = HirTypeUnion(types.toList())

////////////////////////
// Expression helpers //
////////////////////////

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

infix fun Any.hirCatchRaise(that: Any) = HirCatch(this.hir, that.hir, Capture.RAISE)
infix fun Any.hirCatchReturn(that: Any) = HirCatch(this.hir, that.hir, Capture.RETURN)
infix fun Any.hirCatchHandle(that: Any) = HirCatch(this.hir, that.hir, Capture.HANDLE)

val Any.hirNot: HirValue get() = HirNot(hir)
val Any.hirMinus: HirValue get() = HirMinus(hir)
val Any.hirPlus: HirValue get() = HirPlus(hir)

val HirSymbol.hirLoad: HirValue get() = HirLoad(name, emptyList())
fun HirFunction.hirCall(vararg parameters: Any) = HirCall(hirLoad, parameters.map { null hirArg it })
infix fun String?.hirArg(that: Any) = NamedMaybe(this, that.hir)

///////////////////////
// Statement helpers //
///////////////////////

infix fun HirVariable.hirAssign(that: Any) = HirAssign(hirLoad, that.hir)

val HirValue.hirEval get() = HirEvaluate(this)
val Any.hirReturnError get() = HirReturnError(hir)
val Any.hirReturnValue get() = HirReturnValue(hir)

fun Any.hirBranch(
    success: List<HirInstruction> = emptyList(),
    failure: List<HirInstruction> = emptyList(),
) = HirBranch(hir, success, failure)

////////////////////
// Symbol helpers //
////////////////////

fun hirFieldOf(
    name: String = UUID.randomUUID().toString(),
    type: HirType = Builtin.INT32_TYPE,
    value: HirValue? = null,
) = HirField(
    id = UUID.randomUUID(),
    name = name,
    type = type,
    value = value,
    visibility = Visibility.PRIVATE,
    assignability = Assignability.FINAL,
)

fun hirFunOf(
    name: String = UUID.randomUUID().toString(),
    value: HirType? = null,
    error: HirType? = null,
    params: List<HirParameter> = emptyList(),
) = HirFunction(
    id = UUID.randomUUID(),
    name = name,
    type = HirTypeCall(value, error, params.map { it.name to it.type }),
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
    type = HirTypeCall(value, null, listOf("" to param.type)),
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

fun hirStructOf(
    name: String = UUID.randomUUID().toString(),
    fields: List<HirField> = emptyList(),
    methods: List<HirMethod> = emptyList(),
    generics: List<HirGeneric> = emptyList(),
) = HirStruct(
    id = UUID.randomUUID(),
    name = name,
    visibility = Visibility.PRIVATE,
    fields = fields,
    methods = methods,
    generics = generics,
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
