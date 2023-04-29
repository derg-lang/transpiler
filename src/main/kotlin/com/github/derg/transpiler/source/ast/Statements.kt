package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.Mutability
import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.Visibility

/**
 * Expressions may be turned into l-values by being stored in a variable of any kind. In doing so, the r-value
 * expression may be referenced or otherwise accessed again at any later time. All statements which alters the value of
 * a variable is considered to be an assignment.
 */
sealed interface Assignment : Statement
{
    /**
     * Assigns the given [expression] to [name], returning the new value of [name].
     */
    data class Assign(val name: Name, val expression: Expression) : Assignment
}

/**
 * The flow of the program is determined by the control structures. All control structures allows execution to either
 * break out of the current function, enter another function, branch, loop, and anything else the programmer may desire.
 */
sealed interface Control : Statement
{
    /**
     * Splits the execution based on the [predicate]. If the predicate returns a `true` value, the [success] path is
     * chosen, otherwise the [failure] path is chosen (if specified).
     */
    data class Branch(val predicate: Expression, val success: Scope, val failure: Scope?) : Control
    
    /**
     * While ordinarily expressions are not permitted to be statements, functions may be invoked directly as statements
     * under certain circumstances. The [expression] is not permitted to resolve down to any value, it must resolve to
     * something which does not have a type, and does not raise any error.
     */
    data class Invoke(val expression: Expression) : Control
    
    /**
     * Specifies that the execution flow should exit the current function, returning control flow back to the caller.
     * Note that the function cannot be exited in this manner if the function expects a return value.
     */
    // TODO: Add exit statement
    // object Exit : Control
    
    /**
     * Specifies that a function should raise a specific [expression]. The raise statement functions similarly to the
     * return statement, in that execution is resumed to the caller of the function. When a value is raised from a
     * function, this usually indicates that the function has failed to uphold its contract and produced a value which
     * does not conform to the expectations of the caller.
     */
    data class Raise(val expression: Expression) : Control
    
    /**
     * Specifies that a function should return a specific [expression]. The return statement marks the point where the
     * execution is resumed to the caller of the function, executing nothing else in the body of the function. Returning
     * a value from a function indicates usually that the function has succeeded in producing a usable value.
     */
    data class Return(val expression: Expression?) : Control
}

/**
 * Structural components within the source code must be defined to describe their behavior fully. While certain objects
 * such as functions may be declared before being defined, all such objects must be defined before being used. The
 * definition of the object varies from object to object.
 */
sealed interface Definition : Statement
{
    /**
     * Functions are smaller subroutines of a program which can perform a specialized workload, that are given a [name].
     * Every function may return a [valueType], or raise an [errorType]. The value and error types are not required to
     * be specified. Functions accept any number of [parameters], which allows different outcomes of invoking the
     * function to take place.
     *
     * @param visibility The visibility of the function, to whom it is possible to access.
     * @param statements The executable code which defines the function body.
     */
    data class Function(
        val name: Name,
        val valueType: Name?,
        val errorType: Name?,
        val parameters: List<Parameter>,
        val visibility: Visibility,
        val statements: List<Statement>,
    ) : Definition
    
    /**
     * All data structures are represented as types, which determines the shape of all data within a program. Types are
     * not values within the program, but represents the shapes of values. This shape is given a [name], and may hold
     * any number of additional [properties].
     *
     * @param visibility The visibility of the type, to whom it is possible to access.
     */
    data class Type(
        val name: Name,
        val visibility: Visibility,
        val properties: List<Property>,
    ) : Definition
    
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
}
