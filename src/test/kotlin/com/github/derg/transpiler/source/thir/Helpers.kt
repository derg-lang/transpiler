package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import java.util.*

/////////////////////
// Literal helpers //
/////////////////////

val Any.thir: ThirValue
    get() = when (this)
    {
        is ThirValue -> this
        is Boolean   -> ThirConstBool(this)
        is Int       -> ThirConstInt32(this)
        is Long      -> ThirConstInt64(this)
        else         -> throw IllegalArgumentException("Value $this does not represent a valid thir value")
    }

//////////////////
// Type helpers //
//////////////////

fun thirTypeStruct(
    struct: ThirStruct,
    mutability: Mutability = Mutability.IMMUTABLE,
) = ThirType.Structure(
    symbolId = struct.id,
    mutability = mutability,
    parameters = emptyList(),
)

fun thirTypeStruct(
    symbolId: UUID = UUID.randomUUID(),
    mutability: Mutability = Mutability.IMMUTABLE,
) = ThirType.Structure(
    symbolId = symbolId,
    mutability = mutability,
    parameters = emptyList(),
)

fun thirTypeFun(
    value: ThirType? = null,
    error: ThirType? = null,
    parameters: List<ThirType> = emptyList(),
) = ThirType.Function(
    value = value,
    error = error,
    parameters = parameters.map { thirTypeParam(type = it) },
)

fun thirTypeParam(
    name: String = UUID.randomUUID().toString(),
    type: ThirType,
    value: ThirValue? = null,
    passability: Passability = Passability.IN,
) = ThirParameterDynamic(
    name = name,
    type = type,
    value = value,
    passability = passability,
)

////////////////////////
// Expression helpers //
////////////////////////

/**
 * Invokes the [function], assuming it returns a type of the given [value] and [error]. Any number of [params] can be
 * specified, although two should be specified for binary operators, and one for unary operators.
 */
private fun op(function: HirFunction, value: HirStruct, error: HirStruct?, vararg params: ThirValue): ThirCall
{
    val names = if (params.size == 1) mutableMapOf(0 to "rhs") else mutableMapOf(0 to "lhs", 1 to "rhs")
    
    val valueType = ThirType.Structure(value.id, Mutability.IMMUTABLE, emptyList())
    val errorType = error?.let { ThirType.Structure(it.id, Mutability.IMMUTABLE, emptyList()) }
    val callable = ThirType.Function(valueType, errorType, params.mapIndexed { i, p -> thirTypeParam(type = p.value!!, name = names[i]!!) })
    val instance = ThirLoad(callable, function.id, emptyList())
    
    return ThirCall(valueType, errorType, instance, params.toList())
}

val Boolean.thirNot get() = op(Builtin.BOOL_NOT, Builtin.BOOL, null, this.thir)
val Int.thirPlus get() = op(Builtin.INT32_POS, Builtin.INT32, null, this.thir)
val Long.thirPlus get() = op(Builtin.INT64_POS, Builtin.INT64, null, this.thir)
val Int.thirMinus get() = op(Builtin.INT32_NEG, Builtin.INT32, null, this.thir)
val Long.thirMinus get() = op(Builtin.INT64_NEG, Builtin.INT64, null, this.thir)

infix fun Boolean.thirEq(that: Boolean) = op(Builtin.BOOL_EQ, Builtin.BOOL, null, this.thir, that.thir)
infix fun Int.thirEq(that: Int) = op(Builtin.INT32_EQ, Builtin.BOOL, null, this.thir, that.thir)
infix fun Long.thirEq(that: Long) = op(Builtin.INT64_EQ, Builtin.BOOL, null, this.thir, that.thir)
infix fun Boolean.thirNe(that: Boolean) = op(Builtin.BOOL_NE, Builtin.BOOL, null, this.thir, that.thir)
infix fun Int.thirNe(that: Int) = op(Builtin.INT32_NE, Builtin.BOOL, null, this.thir, that.thir)
infix fun Long.thirNe(that: Long) = op(Builtin.INT64_NE, Builtin.BOOL, null, this.thir, that.thir)
infix fun Int.thirGe(that: Int) = op(Builtin.INT32_GE, Builtin.BOOL, null, this.thir, that.thir)
infix fun Long.thirGe(that: Long) = op(Builtin.INT64_GE, Builtin.BOOL, null, this.thir, that.thir)
infix fun Int.thirGt(that: Int) = op(Builtin.INT32_GT, Builtin.BOOL, null, this.thir, that.thir)
infix fun Long.thirGt(that: Long) = op(Builtin.INT64_GT, Builtin.BOOL, null, this.thir, that.thir)
infix fun Int.thirLe(that: Int) = op(Builtin.INT32_LE, Builtin.BOOL, null, this.thir, that.thir)
infix fun Long.thirLe(that: Long) = op(Builtin.INT64_LE, Builtin.BOOL, null, this.thir, that.thir)
infix fun Int.thirLt(that: Int) = op(Builtin.INT32_LT, Builtin.BOOL, null, this.thir, that.thir)
infix fun Long.thirLt(that: Long) = op(Builtin.INT64_LT, Builtin.BOOL, null, this.thir, that.thir)

infix fun Int.thirAdd(that: Int) = op(Builtin.INT32_ADD, Builtin.INT32, null, this.thir, that.thir)
infix fun Long.thirAdd(that: Long) = op(Builtin.INT64_ADD, Builtin.INT64, null, this.thir, that.thir)
infix fun Int.thirSub(that: Int) = op(Builtin.INT32_SUB, Builtin.INT32, null, this.thir, that.thir)
infix fun Long.thirSub(that: Long) = op(Builtin.INT64_SUB, Builtin.INT64, null, this.thir, that.thir)
infix fun Int.thirMul(that: Int) = op(Builtin.INT32_MUL, Builtin.INT32, null, this.thir, that.thir)
infix fun Long.thirMul(that: Long) = op(Builtin.INT64_MUL, Builtin.INT64, null, this.thir, that.thir)
infix fun Int.thirDiv(that: Int) = op(Builtin.INT32_DIV, Builtin.INT32, Builtin.DIVIDE_BY_ZERO, this.thir, that.thir)
infix fun Long.thirDiv(that: Long) = op(Builtin.INT64_DIV, Builtin.INT64, Builtin.DIVIDE_BY_ZERO, this.thir, that.thir)
infix fun Int.thirMod(that: Int) = op(Builtin.INT32_MOD, Builtin.INT32, Builtin.DIVIDE_BY_ZERO, this.thir, that.thir)
infix fun Long.thirMod(that: Long) = op(Builtin.INT64_MOD, Builtin.INT64, Builtin.DIVIDE_BY_ZERO, this.thir, that.thir)

infix fun Boolean.thirAnd(that: Boolean) = op(Builtin.BOOL_AND, Builtin.BOOL, null, this.thir, that.thir)
infix fun Boolean.thirOr(that: Boolean) = op(Builtin.BOOL_OR, Builtin.BOOL, null, this.thir, that.thir)
infix fun Boolean.thirXor(that: Boolean) = op(Builtin.BOOL_XOR, Builtin.BOOL, null, this.thir, that.thir)

infix fun Any.thirCatchRaise(that: Any) = ThirCatch(this.thir, that.thir, Capture.RAISE)
infix fun Any.thirCatchReturn(that: Any) = ThirCatch(this.thir, that.thir, Capture.RETURN)
infix fun Any.thirCatchHandle(that: Any) = ThirCatch(this.thir, that.thir, Capture.HANDLE)

val ThirVariable.thirLoad: ThirValue get() = ThirLoad(type, id, emptyList())
val ThirParameter.thirLoad: ThirValue get() = ThirLoad(type, id, emptyList())
val ThirFunction.thirLoad: ThirValue get() = ThirLoad(type, id, emptyList())
fun ThirValue.thirCall(value: ThirType? = null, error: ThirType? = null, parameters: List<ThirValue> = emptyList()) = ThirCall(value, error, this, parameters)
fun ThirFunction.thirCall(vararg parameters: Any): ThirCall = ThirCall(type.value, type.error, thirLoad, parameters.map { it.thir })

///////////////////////
// Statement helpers //
///////////////////////

infix fun ThirVariable.thirAssign(that: Any) = ThirAssign(id, that.thir)

val Any.thirEval get() = ThirEvaluate(thir)
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
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    type: ThirType? = null,
    value: ThirValue? = null,
) = ThirField(
    id = id,
    name = name,
    type = type ?: value?.value ?: throw IllegalArgumentException("Either type or value must be specified"),
    value = value,
    visibility = Visibility.PRIVATE,
    assignability = Assignability.FINAL,
)

fun thirFunOf(
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    value: ThirType? = null,
    error: ThirType? = null,
    params: List<ThirParameter> = emptyList(),
) = ThirFunction(
    id = id,
    name = name,
    type = ThirType.Function(value, error, params.map { thirTypeParam(name = it.name, type = it.type) }),
    visibility = Visibility.PRIVATE,
    instructions = emptyList(),
    genericIds = emptyList(),
    variableIds = emptyList(),
    parameterIds = params.map { it.id },
)

fun thirLitOf(
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    value: ThirType = thirTypeStruct(Builtin.INT32.id),
    param: ThirParameter = thirParamOf(),
) = ThirFunction(
    id = id,
    name = name,
    type = ThirType.Function(value, null, listOf(thirTypeParam(name = param.name, type = param.type))),
    visibility = Visibility.PRIVATE,
    instructions = emptyList(),
    genericIds = emptyList(),
    variableIds = emptyList(),
    parameterIds = listOf(param.id),
)

fun thirParamOf(
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    type: ThirType = thirTypeStruct(Builtin.INT32.id),
    value: ThirValue? = null,
) = ThirParameter(
    id = id,
    name = name,
    type = type,
    value = value,
    passability = Passability.IN,
)

fun thirStructOf(
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    fields: Set<UUID> = emptySet(),
    methods: Set<UUID> = emptySet(),
    generics: Set<UUID> = emptySet(),
) = ThirStruct(
    id = id,
    name = name,
    visibility = Visibility.PRIVATE,
    fieldIds = fields,
    methodIds = methods,
    genericIds = generics,
)

fun thirVarOf(
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    type: ThirType? = null,
    value: ThirValue? = null,
) = ThirVariable(
    id = id,
    name = name,
    type = type ?: value?.value ?: throw IllegalArgumentException("Either type or value must be specified"),
    assignability = Assignability.FINAL,
)
