package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*

/**
 * All values the source code operates on, are represented as expressions. Expressions may be constant values provided
 * by the developers, parameters passed into functions, intermediary computations of sub-expressions, evaluations of a
 * function call, and more.
 */
sealed interface ThirValue
{
    val valueType: ThirType?
    val errorType: ThirType?
}

/**
 * Represents a value read from memory, or any other named location. The value is read from the location defined by the
 * [instance].
 */
data class ThirLoad(
    val instance: ThirInstance,
    override val valueType: ThirType,
) : ThirValue
{
    override val errorType: Nothing? get() = null
}

/**
 * Invokes the callable [instance] using the provided [parameters]. The instance must be fully disambiguated before the
 * value can be computed.
 */
data class ThirCall(
    val instance: ThirInstance,
    val parameters: List<ThirValue>,
    override val valueType: ThirType?,
    override val errorType: ThirType?,
) : ThirValue

/**
 * Represents the outcome of capturing an error in [lhs], and replacing the error case with the value in [rhs].
 * Depending on the [capture] mode, the resulting value is used either in the original expression in place of the
 * success value, or raised/returned from the callable object.
 */
data class ThirCatch(val lhs: ThirValue, val rhs: ThirValue, val capture: Capture) : ThirValue
{
    override val valueType: ThirType? get() = lhs.valueType
    override val errorType: Nothing? get() = null
}

/**
 * Boolean values, `true` and `false`.
 */
data class ThirConstBool(val raw: Boolean) : ThirValue
{
    override val valueType: ThirType get() = ThirType.Data(Builtin.BOOL.id, Mutability.IMMUTABLE, emptyList())
    override val errorType: Nothing? get() = null
}

/**
 * 32-bit signed integers.
 */
data class ThirConstInt32(val raw: Int) : ThirValue
{
    override val valueType: ThirType get() = ThirType.Data(Builtin.INT32.id, Mutability.IMMUTABLE, emptyList())
    override val errorType: Nothing? get() = null
}

/**
 * 64-bit signed integers.
 */
data class ThirConstInt64(val raw: Long) : ThirValue
{
    override val valueType: ThirType get() = ThirType.Data(Builtin.INT64.id, Mutability.IMMUTABLE, emptyList())
    override val errorType: Nothing? get() = null
}
