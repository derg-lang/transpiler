package com.github.derg.transpiler.ast

import com.github.derg.transpiler.core.Name

/**
 * Variables are units which hold a specific [value] and associates the value with a specific [name]. Variables may
 * optionally be given a [type], which is verified against the actual type of the expression. If the [type] is not
 * specified, it is inferred from [value].
 *
 * @param visibility The visibility of the variable, to whom it is possible to access.
 * @param mutability The kind of the variable, to which degree it is mutable.
 */
data class Variable(
    val name: Name,
    val type: Name?,
    val value: Expression,
    val visibility: Visibility,
    val mutability: Mutability,
) : Definition

/**
 * Functions are smaller subroutines of a program which can perform a specialized workload, that are given a [name].
 * Every function may return a [valueType], or raise an [errorType]. The value and error types are not required to be
 * specified. Functions accept any number of [parameters], which allows different outcomes of invoking the function to
 * take place.
 *
 * @param visibility The visibility of the variable, to whom it is possible to access.
 * @param scope The executable code which defines the function body.
 */
data class Function(
    val name: Name,
    val valueType: Name?,
    val errorType: Name?,
    val parameters: List<Parameter>,
    val visibility: Visibility,
    val scope: Scope,
) : Definition
{
    /**
     * Every function may have any number of parameters, each with their own [name], optional [type] information and
     * optional default [value]. Parameters must contain some degree of [mutability] specifier.
     */
    data class Parameter(
        val name: Name,
        val type: Name?,
        val value: Expression?,
        val mutability: Mutability,
    )
}

/**
 * Every source code file is parsed into a single segment, in which the total collection of segments with the same
 * [module] name form a single module. The segment forms the most basic building block when structuring code, and is the
 * component which allows code fragmentation to take place.
 *
 * @param imports The modules which are to be imported into this segment.
 * @param statements All components which are injected into the module by this segment.
 */
data class Segment(val module: Name?, val imports: Set<Name>, val statements: List<Definition>) : Definition
