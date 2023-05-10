package com.github.derg.transpiler.source

import java.util.*

/**
 * Many objects are given a name which identifies them in some meaningful manner.
 */
typealias Name = String

/**
 * All symbols must be granted a unique id to be able to resolve any identifier to an appropriate object. This id will
 * uniquely identify not only the specific object, but also the specific type of object.
 */
typealias Id = UUID

/**
 * The id provider is a source for arbitrary ids.
 */
interface IdProvider
{
    fun random(): Id
}

/**
 * The default implementation for providing random ids.
 */
object IdProviderSystem : IdProvider
{
    override fun random(): Id = Id.randomUUID()
}

/**
 * The fixed id provider generates the same nil id with every instantiation.
 */
object IdProviderNil : IdProvider
{
    override fun random(): Id = Id(0L, 0L)
}
