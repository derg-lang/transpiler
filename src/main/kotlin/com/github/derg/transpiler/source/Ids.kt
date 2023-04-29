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
