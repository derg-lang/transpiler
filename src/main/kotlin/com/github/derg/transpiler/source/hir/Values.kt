package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.Id

/**
 * All values the source code operates on, are represented as expressions. Expressions may be constant values provided
 * by the developers, parameters passed into functions, intermediary computations of sub-expressions, evaluations of a
 * function call, and more.
 */
sealed interface Value
{
    /**
     * The type id of the value. The type must always be possible to determine for any value, when resolved.
     */
    val type: Id
}
