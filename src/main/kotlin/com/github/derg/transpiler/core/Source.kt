package com.github.derg.transpiler.core

/**
 * Defines a location of a code snippet within a codebase.
 */
data class Location(val file: String, val cursor: Int, val length: Int)

/**
 * Defines a [data] element which is given a particular [location] within a codebase.
 */
data class Localized<Type : Any>(val location: Location, val data: Type)
