package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.utils.*
import java.math.*

/**
 *
 */
sealed interface HirValue

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Resolutions
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Loads the value bound to the identifier with the given [name]. The identifier may refer to either a value bound to
 * any memory address, or any named symbol. When referring to a function, [generics] may be used to disambiguate which
 * instance is intended.
 */
data class HirLoad(
    val name: String,
    val generics: List<NamedMaybe<HirValue>>,
) : HirValue

/**
 * Invokes the callable [instance] using the provided [parameters]. The arguments are specified in the same order in
 * which they appear in source code. In some cases, the callable cannot be resolved without additional information about
 * the callable template specialization.
 */
data class HirCall(
    val instance: HirValue,
    val parameters: List<NamedMaybe<HirValue>>,
) : HirValue

/**
 * Retrieves the value of the given [field] from the given [instance]. The object may be an object which exists on the
 * stack or heap.
 */
data class HirMember(
    val instance: HirValue,
    val field: HirLoad,
) : HirValue

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

data class HirBool(val value: Boolean) : HirValue
data class HirInteger(val value: BigInteger, val literal: String) : HirValue
data class HirDecimal(val value: BigDecimal, val literal: String) : HirValue
data class HirText(val value: String, val literal: String) : HirValue

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Operators
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Arithmetic
data class HirAdd(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirDiv(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirMod(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirMul(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirSub(val lhs: HirValue, val rhs: HirValue) : HirValue

// Comparison
data class HirEq(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirNe(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirGe(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirGt(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirLe(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirLt(val lhs: HirValue, val rhs: HirValue) : HirValue

// Errors
data class HirCatch(val lhs: HirValue, val rhs: HirValue, val capture: Capture) : HirValue

// Logic
data class HirAnd(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirOr(val lhs: HirValue, val rhs: HirValue) : HirValue
data class HirXor(val lhs: HirValue, val rhs: HirValue) : HirValue

// Unary
data class HirMinus(val rhs: HirValue) : HirValue
data class HirNot(val rhs: HirValue) : HirValue
data class HirPlus(val rhs: HirValue) : HirValue
