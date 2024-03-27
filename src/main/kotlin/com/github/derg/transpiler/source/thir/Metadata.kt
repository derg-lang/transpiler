package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * Type annotations indicates which type a specific object is. All variables must be declared with a specific type,
 * either by the developer or by the compiler. In many cases, the compiler can deduce the type, although the developer
 * must have the option of specifying the type when it is ambiguous.
 */
sealed interface ThirType

/**
 * The struct type describes a concrete type by [symbolId], which may be specialized with any number of [generics].
 */
data class ThirTypeData(val symbolId: UUID, val mutability: Mutability, val generics: List<Named<ThirType>>) : ThirType

/**
 * The function type describes a function returning a [value] and [error] type, with any number of [parameters]. The
 * parameters are required to have a valid value, and are not permitted to have any error type associated with them.
 */
data class ThirTypeCall(val value: ThirType?, val error: ThirType?, val parameters: List<Named<ThirType>>) : ThirType

/**
 * The literal type describes a function returning a [value] type, with exactly one [parameter]. The parameter must be a
 * builtin type.
 */
data class ThirTypeLiteral(
    val value: ThirType,
    val parameter: ThirType,
) : ThirType
