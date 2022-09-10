package com.github.derg.transpiler.ast

import com.github.derg.transpiler.core.Name

/**
 * Encapsulation of data and functionality is achieved through visibility. Certain functionality, behavior, and state
 * may be concealed behind an abstraction, where anything outside the abstraction cannot see the internal behavior or
 * state.
 *
 * Visibilities are hierarchical in nature. Everything which is accessible under the public domain is also accessible by
 * everything which could access protected elements, and similarly for everything which has access to private elements.
 */
enum class Visibility
{
    /**
     * The object is accessible to everything within the same module as the object was declared in. The object will not
     * be visible to anything outside the current module.
     */
    PUBLIC,
    
    /**
     * The object is only accessible to the current type in which the object was declared. For example, a private
     * variable may only be accessed by the object where the variable was declared.
     */
    PRIVATE,
}

/**
 * The kind of variable determines what is permitted regarding the data the variable holds. The kind specifies the
 * variable mutability level, restricting the variable from never-changing, to fully mutable.
 */
enum class Mutability
{
    /**
     * The variable is considered a constant. The value can never change, and the properties associated with the value
     * cannot change either. The variable is considered deeply constant, and the user cannot assume that its memory
     * address will remain constant. All variables which are constant may be inlined, or statically computed to other
     * values at compile time, wherever possible.
     */
    VALUE,
    
    /**
     * The variable is considered unchanging and can never change. However, the variable is still possible to mutate by
     * invoking mutating functions or writing different values to the variable properties.
     */
    VARYING,
    
    /**
     * The variable is considered fully mutable and can change in every imaginable way. The variable may be re-assigned
     * a different value, and mutated by either function calls or by updating the variable properties.
     */
    MUTABLE,
}

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
) : Definition()

/**
 * Functions are smaller subroutines of a program which can perform a specialized workload. Every function may return a
 * [valueType], or raise an [errorType]. Functions accept any number of [parameters]. The value and error types are not
 * required to be specified.
 *
 * @param visibility The visibility of the variable, to whom it is possible to access.
 */
data class Function(
    val name: Name,
    val valueType: Name?,
    val errorType: Name?,
    val parameters: List<FunctionParameter>,
    val visibility: Visibility,
    val scope: Scope,
) : Definition()

/**
 * Every function may have any number of parameters, each with their own [name], optional [type] information and
 * optional default [value]. Parameters must contain some degree of [mutability] specifier.
 */
data class FunctionParameter(
    val name: Name,
    val type: Name?,
    val value: Expression?,
    val mutability: Mutability,
)
