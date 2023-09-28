package com.github.derg.transpiler.source.thir

import java.util.*

/**
 * All symbols must be granted a unique id to be able to resolve any identifier to an appropriate object. This id will
 * uniquely identify not only the specific object, but also the specific type of object.
 */
sealed interface ThirId
{
    /**
     * The backing property for the identifiers. This is the unique value representing one specific symbol among all
     * symbols within the source code.
     */
    val value: UUID
    
    /**
     * Represents an identifier which is fully known at symbol declaration time. The type id will not change once the
     * symbol is fully defined.
     */
    data class Static(override val value: UUID = UUID.randomUUID()) : ThirId
    {
        override fun toString(): String = value.toString()
        override fun hashCode(): Int = value.hashCode()
        override fun equals(other: Any?): Boolean = when (other)
        {
            is ThirId -> value == other.value
            else      -> false
        }
    }
    
    /**
     * Represents an identifier which is not fully known at symbol declaration time. Functions may declare a return
     * value to be auto-deduced, in which case the final return type is not known. The type is resolved once the
     * function is fully defined, at which point all other identifiers which pend on this id being resolved can be
     * resolved too.
     *
     * Typically, this identifier is only applicable to functions; it is assigned when a function has an auto-declared
     * return value and/or error type. Once the function is defined, the type will be resolved and a stable id is
     * available. Other symbols may refer to the return value and/or error types, and will be resolved at the same time
     * once the function is defined.
     */
    class Resolvable : ThirId
    {
        private var _value: UUID? = null
        
        override val value: UUID
            get() = _value ?: throw IllegalStateException("Identifier has not yet been resolved")
        
        /**
         * Resolves this id to be the same as the given [id].
         */
        fun resolve(id: ThirId)
        {
            _value = id.value
        }
        
        override fun toString(): String = _value?.toString() ?: "unresolved"
        override fun hashCode(): Int = _value?.hashCode() ?: 0
        override fun equals(other: Any?): Boolean = when (other)
        {
            is ThirId -> _value == other.value
            else      -> false
        }
    }
}
