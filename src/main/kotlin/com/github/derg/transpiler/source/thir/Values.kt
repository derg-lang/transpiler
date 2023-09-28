package com.github.derg.transpiler.source.thir

/**
 * All values the source code operates on, are represented as expressions. Expressions may be constant values provided
 * by the developers, parameters passed into functions, intermediary computations of sub-expressions, evaluations of a
 * function call, and more.
 */
sealed interface ThirValue
{
    /**
     * The type id of the value. The type must always be possible to determine for any value, when resolved.
     */
    val valType: ThirId
    
    /**
     * The type id of the error. The type must always be possible to determine for any value, when resolved.
     */
    val errType: ThirId
}

/**
 * Represents a value read from memory, or any other named location. The value has a specific [valType], and is read
 * from the location defined by the [symbolId].
 */
data class ThirVariableRead(
    override val valType: ThirId,
    val symbolId: ThirId,
) : ThirValue
{
    override val errType: ThirId get() = Builtin.VOID.id
}

/**
 * Represents a value acquired after invoking a function of any sort. The value has a specific [valType], and is
 * acquired from the function with the given [symbolId]. The function is invoked with the given [arguments].
 */
data class ThirFunctionCall(
    override val valType: ThirId,
    override val errType: ThirId,
    val symbolId: ThirId,
    val arguments: List<ThirArgument>,
) : ThirValue

/**
 * Boolean values, `true` and `false`.
 */
sealed interface ThirValueBool : ThirValue
{
    override val valType: ThirId get() = Builtin.BOOL.id
    override val errType: ThirId get() = Builtin.VOID.id
}

data class ThirBoolConst(val value: Boolean) : ThirValueBool
data class ThirBoolEq(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirBoolNe(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirBoolAnd(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirBoolOr(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirBoolXor(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirBoolNot(val rhs: ThirValue) : ThirValueBool

/**
 * 32-bit signed integers.
 */
sealed interface ThirValueInt32 : ThirValue
{
    override val valType: ThirId get() = Builtin.INT32.id
    override val errType: ThirId get() = Builtin.VOID.id
}

data class ThirInt32Const(val value: Int) : ThirValueInt32
data class ThirInt32Eq(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt32Ne(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt32Le(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt32Lt(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt32Ge(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt32Gt(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt32Add(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt32
data class ThirInt32Sub(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt32
data class ThirInt32Mul(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt32
data class ThirInt32Div(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt32
data class ThirInt32Mod(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt32
data class ThirInt32Neg(val rhs: ThirValue) : ThirValueInt32

/**
 * 64-bit signed integers.
 */
sealed interface ThirValueInt64 : ThirValue
{
    override val valType: ThirId get() = Builtin.INT64.id
    override val errType: ThirId get() = Builtin.VOID.id
}

data class ThirInt64Const(val value: Long) : ThirValueInt64
data class ThirInt64Eq(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt64Ne(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt64Le(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt64Lt(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt64Ge(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt64Gt(val lhs: ThirValue, val rhs: ThirValue) : ThirValueBool
data class ThirInt64Add(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt64
data class ThirInt64Sub(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt64
data class ThirInt64Mul(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt64
data class ThirInt64Div(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt64
data class ThirInt64Mod(val lhs: ThirValue, val rhs: ThirValue) : ThirValueInt64
data class ThirInt64Neg(val rhs: ThirValue) : ThirValueInt64

/**
 * Returns a builtin function which does the same as [this] function call, iff a builtin function exists for it.
 * Otherwise, [this] is returned.
 *
 * Note that the builtin functions must be correctly declared (i.e. types and return values are in proper order). Make
 * sure to keep the builtin functions up-to-date with these builtin values.
 */
internal fun ThirFunctionCall.toBuiltin(): ThirValue = when (symbolId)
{
    // Bool
    Builtin.BOOL_AND.id  -> ThirBoolAnd(arguments[0].value, arguments[1].value)
    Builtin.BOOL_EQ.id   -> ThirBoolEq(arguments[0].value, arguments[1].value)
    Builtin.BOOL_NE.id   -> ThirBoolNe(arguments[0].value, arguments[1].value)
    Builtin.BOOL_NOT.id  -> ThirBoolNot(arguments[0].value)
    Builtin.BOOL_OR.id   -> ThirBoolOr(arguments[0].value, arguments[1].value)
    Builtin.BOOL_XOR.id  -> ThirBoolXor(arguments[0].value, arguments[1].value)
    
    // Int32
    Builtin.INT32_EQ.id  -> ThirInt32Eq(arguments[0].value, arguments[1].value)
    Builtin.INT32_GE.id  -> ThirInt32Ge(arguments[0].value, arguments[1].value)
    Builtin.INT32_GT.id  -> ThirInt32Gt(arguments[0].value, arguments[1].value)
    Builtin.INT32_LE.id  -> ThirInt32Le(arguments[0].value, arguments[1].value)
    Builtin.INT32_LT.id  -> ThirInt32Lt(arguments[0].value, arguments[1].value)
    Builtin.INT32_NE.id  -> ThirInt32Ne(arguments[0].value, arguments[1].value)
    Builtin.INT32_ADD.id -> ThirInt32Add(arguments[0].value, arguments[1].value)
    Builtin.INT32_DIV.id -> ThirInt32Div(arguments[0].value, arguments[1].value)
    Builtin.INT32_MOD.id -> ThirInt32Mod(arguments[0].value, arguments[1].value)
    Builtin.INT32_MUL.id -> ThirInt32Mul(arguments[0].value, arguments[1].value)
    Builtin.INT32_NEG.id -> ThirInt32Neg(arguments[0].value)
    Builtin.INT32_POS.id -> arguments[0].value
    Builtin.INT32_SUB.id -> ThirInt32Sub(arguments[0].value, arguments[1].value)
    
    // Int64
    Builtin.INT64_EQ.id  -> ThirInt64Eq(arguments[0].value, arguments[1].value)
    Builtin.INT64_GE.id  -> ThirInt64Ge(arguments[0].value, arguments[1].value)
    Builtin.INT64_GT.id  -> ThirInt64Gt(arguments[0].value, arguments[1].value)
    Builtin.INT64_LE.id  -> ThirInt64Le(arguments[0].value, arguments[1].value)
    Builtin.INT64_LT.id  -> ThirInt64Lt(arguments[0].value, arguments[1].value)
    Builtin.INT64_NE.id  -> ThirInt64Ne(arguments[0].value, arguments[1].value)
    Builtin.INT64_ADD.id -> ThirInt64Add(arguments[0].value, arguments[1].value)
    Builtin.INT64_DIV.id -> ThirInt64Div(arguments[0].value, arguments[1].value)
    Builtin.INT64_MOD.id -> ThirInt64Mod(arguments[0].value, arguments[1].value)
    Builtin.INT64_MUL.id -> ThirInt64Mul(arguments[0].value, arguments[1].value)
    Builtin.INT64_NEG.id -> ThirInt64Neg(arguments[0].value)
    Builtin.INT64_POS.id -> arguments[0].value
    Builtin.INT64_SUB.id -> ThirInt64Sub(arguments[0].value, arguments[1].value)
    
    // Custom function
    else                 -> this
}
