package com.github.derg.transpiler.source.hir

/**
 * All values the source code operates on, are represented as expressions. Expressions may be constant values provided
 * by the developers, parameters passed into functions, intermediary computations of sub-expressions, evaluations of a
 * function call, and more.
 */
sealed interface Value
{
    /**
     * The type id of the value. The type must always be possible to determine for any value, when resolved.
     */
    val type: Type
}

/**
 * Boolean values, `true` and `false`.
 */
sealed interface ValueBool : Value
{
    override val type: Type
        get() = Builtin.BOOL
}

data class BoolConst(val value: Boolean) : ValueBool
data class BoolRead(val variable: Variable) : ValueBool
data class BoolCall(val function: Function, val params: List<Value>) : ValueBool
data class BoolEq(val lhs: ValueBool, val rhs: ValueBool) : ValueBool
data class BoolNe(val lhs: ValueBool, val rhs: ValueBool) : ValueBool
data class BoolAnd(val lhs: ValueBool, val rhs: ValueBool) : ValueBool
data class BoolOr(val lhs: ValueBool, val rhs: ValueBool) : ValueBool
data class BoolXor(val lhs: ValueBool, val rhs: ValueBool) : ValueBool
data class BoolNot(val rhs: ValueBool) : ValueBool

/**
 * 32-bit signed integers.
 */
sealed interface ValueInt32 : Value
{
    override val type: Type
        get() = Builtin.INT32
}

data class Int32Const(val value: Int) : ValueInt32
data class Int32Read(val variable: Variable) : ValueInt32
data class Int32Call(val function: Function, val params: List<Value>) : ValueInt32
data class Int32Eq(val lhs: ValueInt32, val rhs: ValueInt32) : ValueBool
data class Int32Ne(val lhs: ValueInt32, val rhs: ValueInt32) : ValueBool
data class Int32Le(val lhs: ValueInt32, val rhs: ValueInt32) : ValueBool
data class Int32Lt(val lhs: ValueInt32, val rhs: ValueInt32) : ValueBool
data class Int32Ge(val lhs: ValueInt32, val rhs: ValueInt32) : ValueBool
data class Int32Gt(val lhs: ValueInt32, val rhs: ValueInt32) : ValueBool
data class Int32Add(val lhs: ValueInt32, val rhs: ValueInt32) : ValueInt32
data class Int32Sub(val lhs: ValueInt32, val rhs: ValueInt32) : ValueInt32
data class Int32Mul(val lhs: ValueInt32, val rhs: ValueInt32) : ValueInt32
data class Int32Div(val lhs: ValueInt32, val rhs: ValueInt32) : ValueInt32
data class Int32Mod(val lhs: ValueInt32, val rhs: ValueInt32) : ValueInt32
data class Int32Neg(val rhs: ValueInt32) : ValueInt32

/**
 * 64-bit signed integers.
 */
sealed interface ValueInt64 : Value
{
    override val type: Type
        get() = Builtin.INT64
}

data class Int64Const(val value: Long) : ValueInt64
data class Int64Read(val variable: Variable) : ValueInt64
data class Int64Call(val function: Function, val params: List<Value>) : ValueInt64
data class Int64Eq(val lhs: ValueInt64, val rhs: ValueInt64) : ValueBool
data class Int64Ne(val lhs: ValueInt64, val rhs: ValueInt64) : ValueBool
data class Int64Le(val lhs: ValueInt64, val rhs: ValueInt64) : ValueBool
data class Int64Lt(val lhs: ValueInt64, val rhs: ValueInt64) : ValueBool
data class Int64Ge(val lhs: ValueInt64, val rhs: ValueInt64) : ValueBool
data class Int64Gt(val lhs: ValueInt64, val rhs: ValueInt64) : ValueBool
data class Int64Add(val lhs: ValueInt64, val rhs: ValueInt64) : ValueInt64
data class Int64Sub(val lhs: ValueInt64, val rhs: ValueInt64) : ValueInt64
data class Int64Mul(val lhs: ValueInt64, val rhs: ValueInt64) : ValueInt64
data class Int64Div(val lhs: ValueInt64, val rhs: ValueInt64) : ValueInt64
data class Int64Mod(val lhs: ValueInt64, val rhs: ValueInt64) : ValueInt64
data class Int64Neg(val rhs: ValueInt64) : ValueInt64

/**
 * Any value which is produced from user-defined objects.
 */
sealed interface ValueUserDefined : Value

data class UserDefinedRead(val variable: Variable) : ValueUserDefined
{
    override val type: Type
        get() = variable.type
}

data class UserDefinedCall(val function: Function, val params: List<Value>) : ValueUserDefined
{
    override val type: Type
        get() = function.value
}
