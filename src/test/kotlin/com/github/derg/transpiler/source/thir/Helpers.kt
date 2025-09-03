package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import java.util.*

/////////////////////
// Literal helpers //
/////////////////////

val Any.thir: ThirExpression
    get() = when (this)
    {
        is ThirExpression -> this
        is Boolean        -> ThirExpression.Bool(this)
        is Int            -> ThirExpression.Int32(this)
        is Long           -> ThirExpression.Int64(this)
        is Float          -> ThirExpression.Float32(this)
        is Double         -> ThirExpression.Float64(this)
        is String         -> ThirExpression.Str(this)
        else              -> throw IllegalArgumentException("Value $this does not represent a valid thir value")
    }

////////////////////////
// Expression helpers //
////////////////////////

/**
 * Generates a function call for the given [function] and provided input [parameters].
 */
private fun op(function: ThirDeclaration.Function, vararg parameters: ThirExpression): ThirExpression
{
    val instance = ThirExpression.Load(function.id, function.type)
    return ThirExpression.Call(instance, parameters.toList(), function.valueType, function.errorType)
}

val Boolean.thirNot get() = op(Builtin.BOOL_NOT, this.thir)
val Int.thirPlus get() = op(Builtin.INT32_POS, this.thir)
val Long.thirPlus get() = op(Builtin.INT64_POS, this.thir)
val Float.thirPlus get() = op(Builtin.FLOAT32_POS, this.thir)
val Double.thirPlus get() = op(Builtin.FLOAT64_POS, this.thir)
val Int.thirMinus get() = op(Builtin.INT32_NEG, this.thir)
val Long.thirMinus get() = op(Builtin.INT64_NEG, this.thir)
val Float.thirMinus get() = op(Builtin.FLOAT32_NEG, this.thir)
val Double.thirMinus get() = op(Builtin.FLOAT64_NEG, this.thir)

infix fun Boolean.thirEq(that: Boolean) = op(Builtin.BOOL_EQ, this.thir, that.thir)
infix fun Int.thirEq(that: Int) = op(Builtin.INT32_EQ, this.thir, that.thir)
infix fun Long.thirEq(that: Long) = op(Builtin.INT64_EQ, this.thir, that.thir)
infix fun Float.thirEq(that: Float) = op(Builtin.FLOAT32_EQ, this.thir, that.thir)
infix fun Double.thirEq(that: Double) = op(Builtin.FLOAT64_EQ, this.thir, that.thir)
infix fun String.thirEq(that: String) = op(Builtin.STR_EQ, this.thir, that.thir)
infix fun Boolean.thirNe(that: Boolean) = op(Builtin.BOOL_NE, this.thir, that.thir)
infix fun Int.thirNe(that: Int) = op(Builtin.INT32_NE, this.thir, that.thir)
infix fun Long.thirNe(that: Long) = op(Builtin.INT64_NE, this.thir, that.thir)
infix fun Float.thirNe(that: Float) = op(Builtin.FLOAT32_NE, this.thir, that.thir)
infix fun Double.thirNe(that: Double) = op(Builtin.FLOAT64_NE, this.thir, that.thir)
infix fun String.thirNe(that: String) = op(Builtin.STR_NE, this.thir, that.thir)
infix fun Int.thirGe(that: Int) = op(Builtin.INT32_GE, this.thir, that.thir)
infix fun Long.thirGe(that: Long) = op(Builtin.INT64_GE, this.thir, that.thir)
infix fun Float.thirGe(that: Float) = op(Builtin.FLOAT32_GE, this.thir, that.thir)
infix fun Double.thirGe(that: Double) = op(Builtin.FLOAT64_GE, this.thir, that.thir)
infix fun Int.thirGt(that: Int) = op(Builtin.INT32_GT, this.thir, that.thir)
infix fun Long.thirGt(that: Long) = op(Builtin.INT64_GT, this.thir, that.thir)
infix fun Float.thirGt(that: Float) = op(Builtin.FLOAT32_GT, this.thir, that.thir)
infix fun Double.thirGt(that: Double) = op(Builtin.FLOAT64_GT, this.thir, that.thir)
infix fun Int.thirLe(that: Int) = op(Builtin.INT32_LE, this.thir, that.thir)
infix fun Long.thirLe(that: Long) = op(Builtin.INT64_LE, this.thir, that.thir)
infix fun Float.thirLe(that: Float) = op(Builtin.FLOAT32_LE, this.thir, that.thir)
infix fun Double.thirLe(that: Double) = op(Builtin.FLOAT64_LE, this.thir, that.thir)
infix fun Int.thirLt(that: Int) = op(Builtin.INT32_LT, this.thir, that.thir)
infix fun Long.thirLt(that: Long) = op(Builtin.INT64_LT, this.thir, that.thir)
infix fun Float.thirLt(that: Float) = op(Builtin.FLOAT32_LT, this.thir, that.thir)
infix fun Double.thirLt(that: Double) = op(Builtin.FLOAT64_LT, this.thir, that.thir)

infix fun Int.thirAdd(that: Int) = op(Builtin.INT32_ADD, this.thir, that.thir)
infix fun Long.thirAdd(that: Long) = op(Builtin.INT64_ADD, this.thir, that.thir)
infix fun Float.thirAdd(that: Float) = op(Builtin.FLOAT32_ADD, this.thir, that.thir)
infix fun Double.thirAdd(that: Double) = op(Builtin.FLOAT64_ADD, this.thir, that.thir)
infix fun String.thirAdd(that: String) = op(Builtin.STR_ADD, this.thir, that.thir)
infix fun Int.thirSub(that: Int) = op(Builtin.INT32_SUB, this.thir, that.thir)
infix fun Long.thirSub(that: Long) = op(Builtin.INT64_SUB, this.thir, that.thir)
infix fun Float.thirSub(that: Float) = op(Builtin.FLOAT32_SUB, this.thir, that.thir)
infix fun Double.thirSub(that: Double) = op(Builtin.FLOAT64_SUB, this.thir, that.thir)
infix fun Int.thirMul(that: Int) = op(Builtin.INT32_MUL, this.thir, that.thir)
infix fun Long.thirMul(that: Long) = op(Builtin.INT64_MUL, this.thir, that.thir)
infix fun Float.thirMul(that: Float) = op(Builtin.FLOAT32_MUL, this.thir, that.thir)
infix fun Double.thirMul(that: Double) = op(Builtin.FLOAT64_MUL, this.thir, that.thir)
infix fun Int.thirDiv(that: Int) = op(Builtin.INT32_DIV, this.thir, that.thir)
infix fun Long.thirDiv(that: Long) = op(Builtin.INT64_DIV, this.thir, that.thir)
infix fun Float.thirDiv(that: Float) = op(Builtin.FLOAT32_DIV, this.thir, that.thir)
infix fun Double.thirDiv(that: Double) = op(Builtin.FLOAT64_DIV, this.thir, that.thir)
infix fun Int.thirMod(that: Int) = op(Builtin.INT32_MOD, this.thir, that.thir)
infix fun Long.thirMod(that: Long) = op(Builtin.INT64_MOD, this.thir, that.thir)
infix fun Float.thirMod(that: Float) = op(Builtin.FLOAT32_MOD, this.thir, that.thir)
infix fun Double.thirMod(that: Double) = op(Builtin.FLOAT64_MOD, this.thir, that.thir)

infix fun Boolean.thirAnd(that: Boolean) = op(Builtin.BOOL_AND, this.thir, that.thir)
infix fun Boolean.thirOr(that: Boolean) = op(Builtin.BOOL_OR, this.thir, that.thir)
infix fun Boolean.thirXor(that: Boolean) = op(Builtin.BOOL_XOR, this.thir, that.thir)

//infix fun Any.thirCatchRaise(that: Any) = ThirCatch(this.thir, that.thir, Capture.RAISE)
//infix fun Any.thirCatchReturn(that: Any) = ThirCatch(this.thir, that.thir, Capture.RETURN)
//infix fun Any.thirCatchHandle(that: Any) = ThirCatch(this.thir, that.thir, Capture.HANDLE)

fun ThirDeclaration.thirIdent(): ThirExpression = ThirExpression.Load(id, type)
fun ThirDeclaration.Function.thirCall(vararg parameters: Any) = ThirExpression.Call(
    instance = thirIdent(),
    parameters = parameters.map { it.thir },
    valueType = valueType,
    errorType = errorType,
)

///////////////////////
// Statement helpers //
///////////////////////

//infix fun ThirVariable.thirAssign(that: Any) = ThirAssign(id, that.thir)

val Any.thirEval get() = ThirStatement.Evaluate(thir)
val Any.thirReturnError get() = ThirStatement.ReturnError(thir)
val Any.thirReturnValue get() = ThirStatement.ReturnValue(thir)

fun Any.thirIf(
    success: List<ThirStatement> = emptyList(),
    failure: List<ThirStatement> = emptyList(),
) = ThirStatement.If(thir, success, failure)

fun Any.thirWhile(
    statements: List<ThirStatement> = emptyList(),
) = ThirStatement.While(thir, statements.toList())

////////////////////
// Symbol helpers //
////////////////////

fun thirConstOf(
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    type: ThirType = ThirType.Int32,
    value: ThirExpression = ThirExpression.Int32(0),
) = ThirDeclaration.Const(
    id = id,
    name = name,
    type = type,
    def = ThirDeclaration.ConstDef(value = value),
)

fun thirFunOf(
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    valueType: ThirType = ThirType.Void,
    errorType: ThirType = ThirType.Void,
    genericTypeIds: List<UUID> = emptyList(),
    genericValueIds: List<UUID> = emptyList(),
    parameterIds: List<UUID> = emptyList(),
    statements: List<ThirStatement> = emptyList(),
) = ThirDeclaration.Function(
    id = id,
    name = name,
    valueType = valueType,
    errorType = errorType,
    genericTypeIds = genericTypeIds,
    genericValueIds = genericValueIds,
    parameterIds = parameterIds,
    def = ThirDeclaration.FunctionDef(statements = statements),
)

fun thirParamOf(
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    passability: Passability = Passability.IN,
    type: ThirType = ThirType.Int32,
    default: ThirExpression? = null,
) = ThirDeclaration.Parameter(
    id = id,
    name = name,
    passability = passability,
    type = type,
    def = ThirDeclaration.ParameterDef(default = default),
)

fun thirStructOf(
    id: UUID = UUID.randomUUID(),
    name: String = UUID.randomUUID().toString(),
    genericTypeIds: List<UUID> = emptyList(),
    genericValueIds: List<UUID> = emptyList(),
    fieldIds: List<UUID> = emptyList(),
) = ThirDeclaration.Structure(
    id = id,
    name = name,
    genericTypeIds = genericTypeIds,
    genericValueIds = genericValueIds,
    fieldIds = fieldIds,
    def = ThirDeclaration.StructureDef(null),
)
