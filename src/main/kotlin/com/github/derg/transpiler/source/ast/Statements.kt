package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.Name

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
    data class Branch(val predicate: Expression, val success: List<Statement>, val failure: List<Statement>) : Control
    
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
