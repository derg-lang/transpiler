package com.github.derg.transpiler.source.thir

import java.util.*

/**
 * The kind of type an object is.
 */
sealed interface ThirKind
{
    /**
     * The kind of nothing. Any symbol which evaluates to this kind cannot ever be permitted to provide any value of any
     * sort.
     */
    data object Nothing : ThirKind
    
    /**
     * The kind of types which are permitted to have values. This is for example "complete" types, such as `double`,
     * `bool`, `int`, `list<int>`, and so on.
     */
    data object Type : ThirKind
    
    /**
     * The kind of values as type parameters. All type parameters of this kind are shaped according to the [type] they
     * are associated with.
     */
    data class Value(val type: ThirType) : ThirKind
}

/**
 * The types which can be expressed in the type system.
 */
sealed interface ThirType
{
    /**
     * The boolean values, can hold either `true` or `false`.
     */
    data object Bool : ThirType
    
    /**
     * Single-precision floating point numbers.
     */
    data object Float32 : ThirType
    
    /**
     * Double-precision floating point numbers.
     */
    data object Float64 : ThirType
    
    /**
     * 32-bit wide signed integer.
     */
    data object Int32 : ThirType
    
    /**
     * 64-bit wide signed integer.
     */
    data object Int64 : ThirType
    
    /**
     * Unicode string.
     */
    data object Str : ThirType
    
    /**
     * The type of function.
     */
    data class Function(
        val functionId: UUID,
        val typeParameters: List<ThirExpression.Canonical>,
        val valueKind: ThirKind,
        val errorKind: ThirKind,
    ) : ThirType
    
    /**
     * The type of structure.
     */
    data class Structure(
        val structureId: UUID,
        val typeParameters: List<ThirExpression.Canonical>,
    ) : ThirType
    
    /**
     * A value representation for type parameters.
     */
    data class TypeParameterRef(
        val typeParameterId: UUID,
    ) : ThirType
}
