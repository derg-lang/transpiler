package com.github.derg.transpiler.source

/**
 * Many objects are given a name which identifies them in some meaningful manner.
 */
typealias Name = String

/**
 * All symbols must be granted a unique id to be able to resolve any identifier to an appropriate object. This id will
 * uniquely identify not only the specific object, but also the specific type of object.
 */
sealed interface Id
{
    /**
     * The underlying numerical id of the resource. The id will be unique across all types of objects - that is, no two
     * objects will ever share the same id.
     */
    val id: Int
}

@JvmInline
value class IdFunction(override val id: Int) : Id

@JvmInline
value class IdModule(override val id: Int) : Id

@JvmInline
value class IdParameter(override val id: Int) : Id

@JvmInline
value class IdType(override val id: Int) : Id

@JvmInline
value class IdVariable(override val id: Int) : Id
