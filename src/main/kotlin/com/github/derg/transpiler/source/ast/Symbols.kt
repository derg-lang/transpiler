package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.*

/**
 * Structural components within the source code must be defined to describe their behavior fully. While certain objects
 * such as functions may be declared before being defined, all such objects must be defined before being used. The
 * definition of the object varies from object to object.
 */
sealed interface AstSymbol
{
    /**
     * The name of the symbol.
     */
    val name: String
}

/**
 * Constants are units which hold a specific [value] and associates the value with a specific [name]. Constants must be
 * given a [type], which is verified against the actual type of the expression.
 *
 * @param visibility The visibility of the variable, to whom it is possible to access.
 */
data class AstConstant(
    override val name: String,
    val type: AstType?,
    val value: AstValue,
    val visibility: Visibility,
) : AstSymbol

/**
 * Functions are smaller subroutines of a program which can perform a specialized workload, that are given a [name].
 * Every function may return a [valueType], or raise an [errorType]. The value and error types are not required to be
 * specified. Functions accept any number of [parameters], which allows different outcomes of invoking the function to
 * take place.
 *
 * @param visibility The visibility of the function, to whom it is possible to access.
 * @param statements The executable code which defines the function body.
 */
data class AstFunction(
    override val name: String,
    val valueType: AstType?,
    val errorType: AstType?,
    val parameters: List<AstParameter>,
    val visibility: Visibility,
    val statements: List<AstInstruction>,
) : AstSymbol

/**
 * All data structures are represented as types, which determines the shape of all data within a program. Types are not
 * values within the program, but represents the shapes of values. This shape is given a [name], and may hold any number
 * of additional [fields].
 *
 * @param visibility The visibility of the type, to whom it is possible to access.
 */
data class AstStruct(
    override val name: String,
    val visibility: Visibility,
    val fields: List<AstProperty>,
    val templates: List<AstTemplate>,
) : AstSymbol

/**
 * Variables are units which hold a specific [value] and associates the value with a specific [name]. Variables may
 * optionally be given a [type], which is verified against the actual type of the expression. If the [type] is not
 * specified, it is inferred from [value].
 *
 * @param visibility The visibility of the variable, to whom it is possible to access.
 */
data class AstVariable(
    override val name: String,
    val type: AstType?,
    val value: AstValue,
    val visibility: Visibility,
    val assignability: Assignability,
) : AstSymbol, AstInstruction

/**
 * Every function may have any number of parameters, each with their own [name], optional [type] information, and
 * optional default [value]. The [passability] determines how parameters must be passed into the callable.
 */
data class AstParameter(
    val name: String,
    val type: AstType,
    val value: AstValue?,
    val passability: Passability,
)

/**
 * Types may contain an arbitrary number of properties, each with their own [name], optional [type] information, and
 * optional default [value]. Either a value must be provided, and/or type information.
 *
 * @param visibility The visibility of the variable, to whom it is possible to access.
 * @param assignability The assignability of the variable, how values are assigned to it.
 */
data class AstProperty(
    val name: String,
    val type: AstType,
    val value: AstValue?,
    val visibility: Visibility,
    val assignability: Assignability,
)
