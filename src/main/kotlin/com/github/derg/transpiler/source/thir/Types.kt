package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
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
 * The data within the type has a certain [mutability], indicating whether the type is internally mutable or not.
 */
data class ThirTypeStruct(val symbolId: UUID, val mutability: Mutability, val generics: List<Generic>) : ThirType
{
    /**
     * Generics represents information which is known at compile-time, and can be substituted for other bits of
     * information on the use-site. Developers may use generics to write code in a general manner, such that it can be
     * used across a vast number of types and values.
     */
    sealed interface Generic
    
    /**
     * The generic represents a [type] which is aliased by the given [name]. All usages of the generic refers to the
     * given type at the use site, allowing a developer to parameterize code across a selection of types.
     */
    data class Type(val name: String, val type: HirType) : Generic
    
    /**
     * The generic represents a [value] which is aliased by the given [name]. All usages of the generic refers to the
     * given value at the use site, allowing a developer to parameterize code across a selection of values.
     */
    data class Value(val name: String, val value: HirValue) : Generic
}

/**
 * The function type describes a function returning a [value] and [error] type, with any number of [parameters]. The
 * parameters are required to have a valid value, and are not permitted to have any error type associated with them.
 */
data class ThirTypeFunction(val value: ThirType?, val error: ThirType?, val parameters: List<Parameter>) : ThirTypeCall
{
    /**
     * The parameter type describes the [name] and [type] of the input parameter. The parameter is used during call
     * resolution, to disambiguate which callable to invoke.
     */
    data class Parameter(val name: String, val type: ThirType)
}

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
