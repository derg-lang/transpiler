package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.Capture
import com.github.derg.transpiler.source.Mutability
import com.github.derg.transpiler.source.hir.Builtin
import java.util.*

/**
 * All values the source code operates on, are represented as expressions. Expressions may be constant values provided
 * by the developers, parameters passed into functions, intermediary computations of sub-expressions, evaluations of a
 * function call, and more.
 */
sealed interface ThirValue
{
    val value: ThirType?
    val error: ThirType?
}

/**
 * Represents a value read from memory, or any other named location. The value is read from the location defined by the
 * [symbolId], utilizing the given [generics] to disambiguate which specialization to load.
 */
data class ThirLoad(
    override val value: ThirType,
    val symbolId: UUID,
    val generics: List<ThirType>,
) : ThirValue
{
    override val error: Nothing? get() = null
}

/**
 * Invokes the callable [instance] using the provided [parameters]. The instance must be fully disambiguated before the
 * value can be computed.
 */
data class ThirCall(
    override val value: ThirType?,
    override val error: ThirType?,
    val instance: ThirValue,
    val parameters: List<ThirValue>,
) : ThirValue

/**
 * Represents the outcome of capturing an error in [lhs], and replacing the error case with the value in [rhs].
 * Depending on the [capture] mode, the resulting value is used either in the original expression in place of the
 * success value, or raised/returned from the callable object.
 */
data class ThirCatch(val lhs: ThirValue, val rhs: ThirValue, val capture: Capture) : ThirValue
{
    override val value: ThirType get() = ThirTypeUnion(listOf(lhs.value, rhs.value).mapNotNull { it })
    override val error: Nothing? get() = null
}

/**
 * Boolean values, `true` and `false`.
 */
sealed interface ThirValueBool : ThirValue
{
    override val value: ThirType get() = ThirTypeData(Builtin.BOOL.id, Mutability.IMMUTABLE, emptyList())
    override val error: Nothing? get() = null
}

data class ThirBoolConst(val raw: Boolean) : ThirValueBool
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
    override val value: ThirType get() = ThirTypeData(Builtin.INT32.id, Mutability.IMMUTABLE, emptyList())
    override val error: ThirType?
        get() = when (this)
        {
            is ThirInt32Div -> ThirTypeData(Builtin.DIVIDE_BY_ZERO.id, Mutability.IMMUTABLE, emptyList())
            is ThirInt32Mod -> ThirTypeData(Builtin.DIVIDE_BY_ZERO.id, Mutability.IMMUTABLE, emptyList())
            else            -> null
        }
}

data class ThirInt32Const(val raw: Int) : ThirValueInt32
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
    override val value: ThirType get() = ThirTypeData(Builtin.INT64.id, Mutability.IMMUTABLE, emptyList())
    override val error: ThirType?
        get() = when (this)
        {
            is ThirInt64Div -> ThirTypeData(Builtin.DIVIDE_BY_ZERO.id, Mutability.IMMUTABLE, emptyList())
            is ThirInt64Mod -> ThirTypeData(Builtin.DIVIDE_BY_ZERO.id, Mutability.IMMUTABLE, emptyList())
            else            -> null
        }
}

data class ThirInt64Const(val raw: Long) : ThirValueInt64
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
internal fun ThirCall.toBuiltin(): ThirValue
{
    // Cannot inline and optimize function calls that are located somewhere in memory.
    if (instance !is ThirLoad)
        return this
    
    return when (instance.symbolId)
    {
        // Bool
        Builtin.BOOL_AND.id  -> ThirBoolAnd(parameters[0], parameters[1])
        Builtin.BOOL_EQ.id   -> ThirBoolEq(parameters[0], parameters[1])
        Builtin.BOOL_NE.id   -> ThirBoolNe(parameters[0], parameters[1])
        Builtin.BOOL_NOT.id  -> ThirBoolNot(parameters[0])
        Builtin.BOOL_OR.id   -> ThirBoolOr(parameters[0], parameters[1])
        Builtin.BOOL_XOR.id  -> ThirBoolXor(parameters[0], parameters[1])
        
        // Int32
        Builtin.INT32_EQ.id  -> ThirInt32Eq(parameters[0], parameters[1])
        Builtin.INT32_GE.id  -> ThirInt32Ge(parameters[0], parameters[1])
        Builtin.INT32_GT.id  -> ThirInt32Gt(parameters[0], parameters[1])
        Builtin.INT32_LE.id  -> ThirInt32Le(parameters[0], parameters[1])
        Builtin.INT32_LT.id  -> ThirInt32Lt(parameters[0], parameters[1])
        Builtin.INT32_NE.id  -> ThirInt32Ne(parameters[0], parameters[1])
        Builtin.INT32_ADD.id -> ThirInt32Add(parameters[0], parameters[1])
        Builtin.INT32_DIV.id -> ThirInt32Div(parameters[0], parameters[1])
        Builtin.INT32_MOD.id -> ThirInt32Mod(parameters[0], parameters[1])
        Builtin.INT32_MUL.id -> ThirInt32Mul(parameters[0], parameters[1])
        Builtin.INT32_NEG.id -> ThirInt32Neg(parameters[0])
        Builtin.INT32_POS.id -> parameters[0]
        Builtin.INT32_SUB.id -> ThirInt32Sub(parameters[0], parameters[1])
        
        // Int64
        Builtin.INT64_EQ.id  -> ThirInt64Eq(parameters[0], parameters[1])
        Builtin.INT64_GE.id  -> ThirInt64Ge(parameters[0], parameters[1])
        Builtin.INT64_GT.id  -> ThirInt64Gt(parameters[0], parameters[1])
        Builtin.INT64_LE.id  -> ThirInt64Le(parameters[0], parameters[1])
        Builtin.INT64_LT.id  -> ThirInt64Lt(parameters[0], parameters[1])
        Builtin.INT64_NE.id  -> ThirInt64Ne(parameters[0], parameters[1])
        Builtin.INT64_ADD.id -> ThirInt64Add(parameters[0], parameters[1])
        Builtin.INT64_DIV.id -> ThirInt64Div(parameters[0], parameters[1])
        Builtin.INT64_MOD.id -> ThirInt64Mod(parameters[0], parameters[1])
        Builtin.INT64_MUL.id -> ThirInt64Mul(parameters[0], parameters[1])
        Builtin.INT64_NEG.id -> ThirInt64Neg(parameters[0])
        Builtin.INT64_POS.id -> parameters[0]
        Builtin.INT64_SUB.id -> ThirInt64Sub(parameters[0], parameters[1])
        
        // Custom function
        else                 -> this
    }
}
