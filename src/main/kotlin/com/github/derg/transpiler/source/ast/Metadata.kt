package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.*

/**
 * When specifying inputs and outputs of callable objects and bindings, the developer must specify which [name] of the
 * type the relevant values must have. The [mutability] of the types must also be specified.
 */
data class AstType(
    val name: String,
    val mutability: Mutability,
)

/**
 * In order to invoke any sort of function, sometimes arguments must be provided. Arguments consist of a single
 * [expression] which specifies which value they have, and optionally the [name] of the parameters the [expression] is
 * associated with. Arguments must be specified in the correct order, although the ordering may be ignored if the
 * arguments are named instead.
 */
data class AstArgument(
    val name: String?,
    val expression: AstValue,
)
