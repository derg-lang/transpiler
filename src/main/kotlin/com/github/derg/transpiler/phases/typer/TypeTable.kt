package com.github.derg.transpiler.phases.typer

import com.github.derg.transpiler.source.thir.*
import java.util.*

/**
 * The type table contains information regarding the various objects which have had their types resolved. When an
 * object's type is resolved, it can be used for all type resolution purposes. For example, the actual callable to
 * invoke can be determined by the parameters, once the types are known.
 */
class TypeTable
{
    val bindings = mutableMapOf<UUID, ThirType>()
    val functions = mutableMapOf<UUID, ThirType.Function>()
    val literals = mutableMapOf<UUID, ThirType.Function>()
}
