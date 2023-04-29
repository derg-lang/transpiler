package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.Assignability
import com.github.derg.transpiler.source.Mutability
import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.Visibility

/**
 * In order to invoke any sort of function, sometimes arguments must be provided. Arguments consist of a single
 * [expression] which specifies which value they have, and optionally the [name] of the parameters the [expression] is
 * associated with. Arguments must be specified in the correct order, although the ordering may be ignored if the
 * arguments are named instead.
 */
data class Argument(
    val name: Name?,
    val expression: Expression,
)

/**
 * Every function may have any number of parameters, each with their own [name], optional [type] information, and
 * optional default [value]. Parameters must contain a [mutability] and [assignability] specifier.
 */
data class Parameter(
    val name: Name,
    val type: Name?,
    val value: Expression?,
    val mutability: Mutability,
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
data class Property(
    val name: Name,
    val type: Name?,
    val value: Expression?,
    val visibility: Visibility,
    val mutability: Mutability,
    val assignability: Assignability,
)

/**
 * The collection of any number of [statements] within a logical unit is known as a scope. The scope permits arbitrary
 * code to be performed, including defining new variables, functions, types, and so on. All executable parts of the
 * program must be located within a scope.
 *
 * @property isBraced Whether the scope is surrounded with braces or not.
 */
data class Scope(
    val isBraced: Boolean,
    val statements: List<Statement>,
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
data class Segment(
    val module: Name?,
    val imports: List<Name>,
    val definitions: List<Definition>,
)
