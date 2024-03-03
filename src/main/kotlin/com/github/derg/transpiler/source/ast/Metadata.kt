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

/**
 * Every source code file is parsed into a single segment, in which the total collection of segments with the same
 * module name form a single module. The segment forms the most basic building block when structuring code, and is the
 * component which allows code fragmentation to take place.
 *
 * @param module The name of the module this segment is part of.
 * @param imports The modules which are to be imported into this segment.
 * @param definitions All components which are injected into the module by this segment.
 */
data class AstSegment(
    val module: String?,
    val imports: List<String>,
    val definitions: List<AstSymbol>,
)
