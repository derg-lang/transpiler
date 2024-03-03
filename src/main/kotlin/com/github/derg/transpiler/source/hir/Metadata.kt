package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*

/**
 * Type annotations indicates which type a specific object is. All variables must be declared with a specific type,
 * either by the developer or by the compiler. In many cases, the compiler can deduce the type, although the developer
 * must have the option of specifying the type when it is ambiguous.
 */
sealed interface HirType

/**
 * The struct type describes a concrete type by [name], which may be specialized with any number of [generics].
 */
// TODO: Is this needed?
data class HirTypeStruct(
    val name: String,
    val generics: List<HirTypedParameter>,
    val mutability: Mutability,
) : HirType

/**
 * The function type describes a function returning a [value] and [error] type, with any number of [parameters]. The
 * parameters are required to have a valid value, and are not permitted to have any error type associated with them.
 */
// TODO: Is this needed?
data class HirTypeFunction(
    val value: HirType?,
    val error: HirType?,
    val parameters: List<HirTypedParameter>,
) : HirType

/**
 * The literal type describes a function returning a [value] type, with exactly one [parameter]. The parameter must be a
 * builtin type.
 */
// TODO: Is this needed?
data class HirTypeLiteral(
    val value: HirType,
    val parameter: HirTypeStruct,
) : HirType

/**
 * The parameter type describing what the type of the parameter is. All function parameters must have a [name] and
 * [value] type.
 */
data class HirTypedParameter(
    val name: String,
    val value: HirType,
)

/**
 * An optionally named parameter used when invoking a function. The parameter may be given a specific [name], but must
 * be given a specific [value].
 */
data class HirNamedParameter(
    val name: String?,
    val value: HirValue,
)
