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
sealed interface ThirTypeCall : ThirType

/**
 * The struct type describes a concrete type by [symbolId], which may be specialized with any number of [generics].
 */
data class ThirTypeStruct(val symbolId: UUID, val mutability: Mutability, val generics: List<Named<ThirType>>) : ThirType

/**
 * The function type describes a function returning a [value] and [error] type, with any number of [parameters]. The
 * parameters are required to have a valid value, and are not permitted to have any error type associated with them.
 */
data class ThirTypeFunction(val value: ThirType?, val error: ThirType?, val parameters: List<Named<ThirType>>) : ThirTypeCall

/**
 * The literal type describes a literal returning a [value] type, taking in exactly one [parameter]. The parameter is
 * required to have a valid value, and is not permitted to have any error type associated with it.
 */
data class ThirTypeLiteral(val value: ThirType, val parameter: ThirTypeStruct) : ThirTypeCall

/**
 * The union type describes a type which is one of the specified [types]. Any type can be in a union with a union type,
 * although the set of potential types is collapsed into a single set.
 */
data class ThirTypeUnion(val types: List<ThirType>) : ThirType
