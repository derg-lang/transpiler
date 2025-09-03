package com.github.derg.transpiler.source.thir

import java.util.*

/**
 * The type environment holds the collection of all known types encountered in a program. All objects encountered in the
 * source code of a program, should be given a type. This information can be used to resolve the entire program,
 * performing type-checking and type-resolution.
 */
class Environment
{
    val declarations = mutableMapOf<UUID, ThirDeclaration>()
}
