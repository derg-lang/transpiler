package com.github.derg.transpiler.experimental.resolver

import com.github.derg.transpiler.utils.*
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
     * Type passed as a value.
     */
    data object Type : ThirType
    
    /**
     * Absence of a value.
     */
    data object Void : ThirType
    
    /////////////////////////////////////////////////////////
    // Composite types, must be handled with special care. //
    /////////////////////////////////////////////////////////
    
    /**
     * Indicates that the expression is either a [valueType] or an [errorType] at runtime. The two channels are used in
     * specific expressions and operators to perform error handling, and can be used in the type system to verify that
     * errors are caught.
     */
    data class Channel(val valueType: ThirType, val errorType: ThirType) : ThirType
    
    /**
     * The type of function.
     */
    data class Function(val parameters: List<NamedMaybe<ThirType>>, val output: ThirType) : ThirType
    
    /**
     * The type of structure.
     */
    data class Instance(val structureId: UUID, val typeParameters: List<ThirExpression.Canonical>) : ThirType
    
    /**
     * The type of tuples.
     */
    data class Tuple(val variants: List<ThirType>) : ThirType
    
    /**
     * A placeholder type variable. Used during type-checking to represent an unresolved type.
     */
    data class Var(val nodeId: UUID) : ThirType
}
