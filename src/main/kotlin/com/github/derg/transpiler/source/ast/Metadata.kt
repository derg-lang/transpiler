package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.*

/**
 * In order to invoke any sort of function, sometimes arguments must be provided. Arguments consist of a single
 * [expression] which specifies which value they have, and optionally the [name] of the parameters the [expression] is
 * associated with. Arguments must be specified in the correct order, although the ordering may be ignored if the
 * arguments are named instead.
 */
data class AstArgument(
    val name: String?,
    val expression: AstExpression,
)

/**
 * Every function may have any number of parameters, each with their own [name], optional [type] information, and
 * optional default [value]. Parameters must contain a [passability] and [assignability] specifier.
 */
data class AstParameter(
    val name: String,
    val type: String,
    val value: AstExpression?,
    val passability: Passability,
    val assignability: Assignability,
)

/**
 * Types may contain an arbitrary number of properties, each with their own [name], optional [type] information, and
 * optional default [value]. Either a value must be provided, and/or type information.
 *
 * @param visibility The visibility of the variable, to whom it is possible to access.
 * @param mutability The kind of the variable, to which degree it is mutable.
 * @param assignability The assignability of the variable, how values are assigned to it.
 */
data class AstProperty(
    val name: String,
    val type: String,
    val value: AstExpression?,
    val visibility: Visibility,
    val mutability: Mutability,
    val assignability: Assignability,
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
    val definitions: List<AstDefinition>,
)
