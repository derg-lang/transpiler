package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.utils.*

/**
 * Type annotations indicates which type a specific object is. All variables must be declared with a specific type,
 * either by the developer or by the compiler. In many cases, the compiler can deduce the type, although the developer
 * must have the option of specifying the type when it is ambiguous.
 */
sealed interface HirType

/**
 * The struct type describes a concrete type by [name], which may be specialized with any number of [generics]. The data
 * within the type has a certain [mutability], indicating whether the type is internally mutable or not.
 */
data class HirTypeStruct(val name: String, val mutability: Mutability, val generics: List<Named<HirType>>) : HirType

/**
 * The function type describes a function returning a [value] and [error] type, with any number of [parameters]. The
 * parameters are required to have a valid value, and are not permitted to have any error type associated with them.
 */
data class HirTypeFunction(val value: HirType?, val error: HirType?, val parameters: List<Named<HirType>>) : HirType

/**
 * The union type describes a type which is one of the specified [types]. Any type can be in a union with a union type,
 * although the set of potential types is collapsed into a single set.
 */
data class HirTypeUnion(val types: List<HirType>) : HirType
