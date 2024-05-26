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
data class ThirConstBool(val raw: Boolean) : ThirValue
{
    override val value: ThirType get() = ThirTypeStruct(Builtin.BOOL.id, Mutability.IMMUTABLE, emptyList())
    override val error: Nothing? get() = null
}

/**
 * 32-bit signed integers.
 */
data class ThirConstInt32(val raw: Int) : ThirValue
{
    override val value: ThirType get() = ThirTypeStruct(Builtin.INT32.id, Mutability.IMMUTABLE, emptyList())
    override val error: Nothing? get() = null
}

/**
 * 64-bit signed integers.
 */
data class ThirConstInt64(val raw: Long) : ThirValue
{
    override val value: ThirType get() = ThirTypeStruct(Builtin.INT64.id, Mutability.IMMUTABLE, emptyList())
    override val error: Nothing? get() = null
}
