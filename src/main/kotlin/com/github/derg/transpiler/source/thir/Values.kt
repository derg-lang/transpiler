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
    val type: ThirType
}

/**
 * Boolean values, `true` and `false`.
 */
sealed interface ThirValueBool : ThirValue
{
    override val type: ThirType
        get() = Builtin.BOOL
}

data class BoolConst(val value: Boolean) : ThirValueBool
data class BoolRead(val variable: ThirVariable) : ThirValueBool
data class BoolCall(val function: ThirFunction, val params: List<ThirValue>) : ThirValueBool
data class BoolEq(val lhs: ThirValueBool, val rhs: ThirValueBool) : ThirValueBool
data class BoolNe(val lhs: ThirValueBool, val rhs: ThirValueBool) : ThirValueBool
data class BoolAnd(val lhs: ThirValueBool, val rhs: ThirValueBool) : ThirValueBool
data class BoolOr(val lhs: ThirValueBool, val rhs: ThirValueBool) : ThirValueBool
data class BoolXor(val lhs: ThirValueBool, val rhs: ThirValueBool) : ThirValueBool
data class BoolNot(val rhs: ThirValueBool) : ThirValueBool

/**
 * 32-bit signed integers.
 */
sealed interface ThirValueInt32 : ThirValue
{
    override val type: ThirType
        get() = Builtin.INT32
}

data class Int32Const(val value: Int) : ThirValueInt32
data class Int32Read(val variable: ThirVariable) : ThirValueInt32
data class Int32Call(val function: ThirFunction, val params: List<ThirValue>) : ThirValueInt32
data class Int32Eq(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueBool
data class Int32Ne(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueBool
data class Int32Le(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueBool
data class Int32Lt(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueBool
data class Int32Ge(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueBool
data class Int32Gt(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueBool
data class Int32Add(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueInt32
data class Int32Sub(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueInt32
data class Int32Mul(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueInt32
data class Int32Div(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueInt32
data class Int32Mod(val lhs: ThirValueInt32, val rhs: ThirValueInt32) : ThirValueInt32
data class Int32Neg(val rhs: ThirValueInt32) : ThirValueInt32

/**
 * 64-bit signed integers.
 */
sealed interface ThirValueInt64 : ThirValue
{
    override val type: ThirType
        get() = Builtin.INT64
}

data class Int64Const(val value: Long) : ThirValueInt64
data class Int64Read(val variable: ThirVariable) : ThirValueInt64
data class Int64Call(val function: ThirFunction, val params: List<ThirValue>) : ThirValueInt64
data class Int64Eq(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueBool
data class Int64Ne(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueBool
data class Int64Le(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueBool
data class Int64Lt(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueBool
data class Int64Ge(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueBool
data class Int64Gt(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueBool
data class Int64Add(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueInt64
data class Int64Sub(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueInt64
data class Int64Mul(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueInt64
data class Int64Div(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueInt64
data class Int64Mod(val lhs: ThirValueInt64, val rhs: ThirValueInt64) : ThirValueInt64
data class Int64Neg(val rhs: ThirValueInt64) : ThirValueInt64

/**
 * Unicode strings.
 */
// TODO: Implement me
object ThirValueStrUnicode : ThirValue
{
    override val type: ThirType
        get() = Builtin.VOID
}

/**
 * Any value which is produced from user-defined objects.
 */
sealed interface ThirValueUserDefined : ThirValue

data class UserDefinedRead(val variable: ThirVariable) : ThirValueUserDefined
{
    override val type: ThirType
        get() = variable.type
}

data class UserDefinedCall(val function: ThirFunction, val params: List<ThirValue>) : ThirValueUserDefined
{
    override val type: ThirType
        get() = function.value
}
