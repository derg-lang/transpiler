package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import java.util.*

/////////////////////
// Literal helpers //
/////////////////////

val Any.thir: ThirValue
    get() = when (this)
    {
        is Boolean -> ThirBoolConst(this)
        is Int     -> ThirInt32Const(this)
        is Long    -> ThirInt64Const(this)
        else       -> throw IllegalArgumentException("Value $this does not represent a valid thir value")
    }

////////////////////////
// Expression helpers //
////////////////////////

val Boolean.thirNot get() = ThirBoolNot(thir)
val Int.thirMinus get() = ThirInt32Neg(thir)
val Long.thirMinus get() = ThirInt64Neg(thir)

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

val ThirVariable.thirRead: ThirValue get() = ThirVariableRead(type, id)
val ThirParameter.thirRead: ThirValue get() = ThirVariableRead(type, id)
fun ThirFunction.thirCall(vararg arguments: Any) = ThirFunctionCall(valType, errType, id, arguments.map(Any::thirArg))

val Any.thirArg: ThirArgument
    get() = if (this is Pair<*, *>) ThirArgument(first as String, (second as Any).thir) else ThirArgument(null, thir)

///////////////////////
// Statement helpers //
///////////////////////

infix fun ThirVariable.thirAssign(that: Any) = ThirAssign(id, that.thir)

val Any.thirReturnError get() = ThirReturnError(thir)
val Any.thirReturnValue get() = ThirReturnValue(thir)
val ThirValue.thirEval get() = ThirEvaluate(this)

////////////////////
// Symbol helpers //
////////////////////

/**
 * Generates a function from the provided input parameters.
 */
fun thirFunOf(
    name: String = UUID.randomUUID().toString(),
    valType: ThirType = Builtin.VOID,
    errType: ThirType = Builtin.VOID,
    params: List<ThirParameter> = emptyList(),
) = ThirFunction(
    id = ThirId.Static(),
    name = name,
    valType = ThirId.Resolvable().apply { resolve(valType.id) },
    errType = ThirId.Resolvable().apply { resolve(errType.id) },
    params = params,
    visibility = Visibility.PRIVATE,
    scope = ThirScope(ThirSymbolTable()),
)

/**
 * Generates a parameter from the provided input parameters.
 */
fun thirParOf(
    type: ThirType,
    name: String = UUID.randomUUID().toString(),
    def: ThirValue? = null,
) = ThirParameter(
    id = ThirId.Static(),
    name = name,
    type = ThirId.Resolvable().apply { resolve(type.id) },
    defaultValue = def,
    passability = Passability.IN,
)

/**
 * Generates a type from the provided input parameters.
 */
fun thirTypeOf(
    name: String = UUID.randomUUID().toString(),
    properties: List<ThirProperty> = listOf(),
) = ThirType(
    id = ThirId.Static(),
    name = name,
    visibility = Visibility.PRIVATE,
    properties = properties,
    scope = ThirScope(ThirSymbolTable()),
)

/**
 * Generates type property definition from the provided input parameters.
 */
fun thirPropOf(
    type: ThirType,
    name: String = UUID.randomUUID().toString(),
    vis: Visibility = Visibility.PRIVATE,
    mut: Mutability = Mutability.IMMUTABLE,
    ass: Assignability = Assignability.CONSTANT,
) = ThirProperty(
    id = ThirId.Static(),
    type = ThirId.Resolvable().apply { resolve(type.id) },
    name = name,
    visibility = vis,
    mutability = mut,
    assignability = ass,
)

/**
 * Generates a variable from the provided input parameters.
 */
fun thirVarOf(
    type: ThirType,
    name: String = UUID.randomUUID().toString(),
) = ThirVariable(
    id = ThirId.Static(),
    name = name,
    type = ThirId.Resolvable().apply { resolve(type.id) },
    visibility = Visibility.PRIVATE,
    mutability = Mutability.IMMUTABLE,
    assignability = Assignability.CONSTANT,
)
