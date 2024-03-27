package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import java.util.*

/////////////////////
// Literal helpers //
/////////////////////

val Any.thir: ThirValue
    get() = when (this)
    {
        is ThirValue -> this
        is Boolean   -> ThirBoolConst(this)
        is Int       -> ThirInt32Const(this)
        is Long      -> ThirInt64Const(this)
        else         -> throw IllegalArgumentException("Value $this does not represent a valid thir value")
    }

////////////////////////
// Expression helpers //
////////////////////////

infix fun Boolean.thirEq(that: Boolean) = ThirBoolEq(this.thir, that.thir)
infix fun Int.thirEq(that: Int) = ThirInt32Eq(this.thir, that.thir)
infix fun Long.thirEq(that: Long) = ThirInt64Eq(this.thir, that.thir)
infix fun Boolean.thirNe(that: Boolean) = ThirBoolNe(this.thir, that.thir)
infix fun Int.thirNe(that: Int) = ThirInt32Ne(this.thir, that.thir)
infix fun Long.thirNe(that: Long) = ThirInt64Ne(this.thir, that.thir)
infix fun Int.thirLe(that: Int) = ThirInt32Le(this.thir, that.thir)
infix fun Long.thirLe(that: Long) = ThirInt64Le(this.thir, that.thir)
infix fun Int.thirLt(that: Int) = ThirInt32Lt(this.thir, that.thir)
infix fun Long.thirLt(that: Long) = ThirInt64Lt(this.thir, that.thir)
infix fun Int.thirGe(that: Int) = ThirInt32Ge(this.thir, that.thir)
infix fun Long.thirGe(that: Long) = ThirInt64Ge(this.thir, that.thir)
infix fun Int.thirGt(that: Int) = ThirInt32Gt(this.thir, that.thir)
infix fun Long.thirGt(that: Long) = ThirInt64Gt(this.thir, that.thir)
infix fun Boolean.thirAnd(that: Boolean) = ThirBoolAnd(this.thir, that.thir)
infix fun Boolean.thirOr(that: Boolean) = ThirBoolOr(this.thir, that.thir)
infix fun Boolean.thirXor(that: Boolean) = ThirBoolXor(this.thir, that.thir)
infix fun Int.thirAdd(that: Int) = ThirInt32Add(this.thir, that.thir)
infix fun Long.thirAdd(that: Long) = ThirInt64Add(this.thir, that.thir)
infix fun Int.thirSub(that: Int) = ThirInt32Sub(this.thir, that.thir)
infix fun Long.thirSub(that: Long) = ThirInt64Sub(this.thir, that.thir)
infix fun Int.thirMul(that: Int) = ThirInt32Mul(this.thir, that.thir)
infix fun Long.thirMul(that: Long) = ThirInt64Mul(this.thir, that.thir)
infix fun Int.thirDiv(that: Int) = ThirInt32Div(this.thir, that.thir)
infix fun Long.thirDiv(that: Long) = ThirInt64Div(this.thir, that.thir)
infix fun Int.thirMod(that: Int) = ThirInt32Mod(this.thir, that.thir)
infix fun Long.thirMod(that: Long) = ThirInt64Mod(this.thir, that.thir)

val Boolean.thirNot get() = ThirBoolNot(thir)
val Int.thirMinus get() = ThirInt32Neg(thir)
val Long.thirMinus get() = ThirInt64Neg(thir)

val ThirVariable.thirLoad: ThirValue get() = ThirLoad(type, id, emptyList())
val ThirParameter.thirLoad: ThirValue get() = ThirLoad(type, id, emptyList())
val ThirFunction.thirLoad: ThirValue get() = ThirLoad(type, id, emptyList())
fun ThirFunction.thirCall(vararg parameters: Any) = ThirCall(type.value, type.error, thirLoad, parameters.map { it.thir })

///////////////////////
// Statement helpers //
///////////////////////

infix fun ThirVariable.thirAssign(that: Any) = ThirAssign(id, that.thir)

val ThirValue.thirEval get() = ThirEvaluate(this)
val Any.thirReturnError get() = ThirReturnError(thir)
val Any.thirReturnValue get() = ThirReturnValue(thir)

fun Any.thirBranch(
    success: List<ThirInstruction> = emptyList(),
    failure: List<ThirInstruction> = emptyList(),
) = ThirBranch(thir, success, failure)

////////////////////
// Symbol helpers //
////////////////////

fun thirFieldOf(
    name: String = UUID.randomUUID().toString(),
    type: ThirType? = null,
    value: ThirValue? = null,
) = ThirField(
    id = UUID.randomUUID(),
    name = name,
    type = type ?: value?.value ?: throw IllegalArgumentException("Either type or value must be specified"),
    value = value,
    visibility = Visibility.PRIVATE,
    assignability = Assignability.FINAL,
)

fun thirFunOf(
    name: String = UUID.randomUUID().toString(),
    value: ThirType? = null,
    error: ThirType? = null,
    params: List<ThirParameter> = emptyList(),
) = ThirFunction(
    id = UUID.randomUUID(),
    name = name,
    type = ThirTypeCall(value, error, params.map { it.name to it.type }),
    visibility = Visibility.PRIVATE,
    instructions = emptyList(),
    genericIds = emptySet(),
    variableIds = emptySet(),
    parameterIds = params.map { it.id }.toSet(),
)

fun thirParamOf(
    name: String = UUID.randomUUID().toString(),
    type: ThirType? = null,
    value: ThirValue? = null,
) = ThirParameter(
    id = UUID.randomUUID(),
    name = name,
    type = type ?: value?.value ?: throw IllegalArgumentException("Either type or value must be specified"),
    value = value,
    passability = Passability.IN,
)

fun thirStructOf(
    name: String = UUID.randomUUID().toString(),
    fields: Set<UUID> = emptySet(),
    methods: Set<UUID> = emptySet(),
    generics: Set<UUID> = emptySet(),
) = ThirStruct(
    id = UUID.randomUUID(),
    name = name,
    visibility = Visibility.PRIVATE,
    fieldIds = fields,
    methodIds = methods,
    genericIds = generics,
)

fun thirVarOf(
    name: String = UUID.randomUUID().toString(),
    type: ThirType? = null,
    value: ThirValue? = null,
) = ThirVariable(
    id = UUID.randomUUID(),
    name = name,
    type = type ?: value?.value ?: throw IllegalArgumentException("Either type or value must be specified"),
    assignability = Assignability.FINAL,
)
