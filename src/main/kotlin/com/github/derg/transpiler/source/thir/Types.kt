package com.github.derg.transpiler.source.thir

import java.util.*

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
     * The type of types, indicates a type is passed as value.
     */
    data object Type : ThirType
    
    /**
     * The unit type, indicates an absence of value.
     */
    data object Void : ThirType
    
    /**
     * User-defined function.
     */
    data class Function(val valueType: ThirType, val errorType: ThirType) : ThirType
    
    /**
     * User-defined structure of the given [id].
     */
    data class Structure(val id: UUID) : ThirType
    
    /**
     * Instance of user-defined structure of the given [id], with the given [types] and [values] wildcards.
     */
    data class Instance(val id: UUID, val types: Map<UUID, ThirType>, val values: Map<UUID, ThirExpression>) : ThirType
}
