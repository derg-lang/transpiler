package com.github.derg.transpiler.ast

import com.github.derg.transpiler.core.Name

/**
 * Any constant in source code, either a number, a string, or a boolean value, are treated specially. Any constant value
 * has the property of never changing, and as such allows optimizations such as constant folding, inlining, and compile
 * time calculations.
 */
sealed class Value : Expression()
{
    /**
     * Represents a `true` or `false` [value]. Booleans cannot hold any other value, and cannot be given a user-defined
     * literal.
     */
    data class Bool(val value: Boolean) : Value()
    
    /**
     * Any real number may be represented in [value]. The precision of the number is limited by the target language and
     * hardware. All numbers may have an optional [type] associated with them, indicating which user-defined literal
     * should be used to interpret the value.
     */
    data class Real(val value: Number, val type: Name?) : Value()
    
    /**
     * All strings may be represented in [value]. All types of text are permitted, although limitations may be imposed
     * by the target language and hardware. All strings may have an optional [type] associated with them, indicating
     * which user-defined literal should be used to interpret the value.
     */
    data class Text(val value: String, val type: Name?) : Value()
}

/**
 * Retrieving values from existing objects in source code is known as accessing the value. Various methods for accessing
 * data are viable, such as reading the value directly from a variable, calculating the value of a function call, or
 * utilizing the subscript operator.
 */
sealed class Access : Expression()
{
    /**
     * All variables are permitted to hold a value, which may be accessed by examining the variable itself. Variables
     * are given a unique [name] within every scope, allowing the analyzer to resolve which variable is being
     * referenced. Any type property will be represented as a variable in the abstract syntax tree.
     */
    data class Variable(val name: Name) : Access()
    
    /**
     * All procedure calls may either refer to a function with the specified [name] being invoked, or the function
     * operator being invoked on an instance with the given [name]. Every procedure call is permitted an arbitrary
     * number of [parameters].
     */
    data class Function(val name: Name, val parameters: List<Parameter>) : Access()
    
    /**
     * All subscript calls refers to the subscript operator being invoked on an instance with the given [name]. Every
     * subscript call is permitted an arbitrary number of [parameters].
     */
    data class Subscript(val name: Name, val parameters: List<Parameter>) : Access()
}

/**
 * Expressions may be turned into l-values by being stored in a variable of any kind. In doing so, the r-value
 * expression may be referenced or otherwise accessed again at any later time. All statements which alters the value of
 * a variable is considered to be an assignment.
 */
sealed class Assignment : Expression()
{
    // INCREMENTING AND DECREMENTING ASSIGNMENTS
    
    /**
     * Performs an increment on the variable with the given [name], returning the value *after* incrementing.
     */
    data class PreIncrement(val name: Name) : Assignment()
    
    /**
     * Performs a decrement on the variable with the given [name], returning the value *after* decrementing.
     */
    data class PreDecrement(val name: Name) : Assignment()
    
    /**
     * Performs an increment on the variable with the given [name], returning the value *before* incrementing.
     */
    data class PostIncrement(val name: Name) : Assignment()
    
    /**
     * Performs a decrement on the variable with the given [name], returning the value *before* decrementing.
     */
    data class PostDecrement(val name: Name) : Assignment()
    
    // ASSIGNMENT ASSIGNMENTS
    
    /**
     * Assigns the given [expression] to [name], returning the new value of [name].
     */
    data class Assign(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Adds the given [expression] to [name], returning the new value of [name].
     */
    data class AssignAdd(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Subtracts the given [expression] from [name], returning the new value of [name].
     */
    data class AssignSubtract(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Multiplies [name] with the given [expression], returning the new value of [name].
     */
    data class AssignMultiply(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Divides [name] by the given [expression], returning the new value of [name].
     */
    data class AssignDivide(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Assigns the modulo of [name] with the given [expression], returning the new value of [name].
     */
    data class AssignModulo(val name: Name, val expression: Expression) : Assignment()
}

/**
 * Any two expressions may be combined with an operator to form a new value. The operator determines which operation
 * will be performed on the expressions participating in the operation. Certain operations require only one expression,
 * some require two.
 *
 * Prefix operators operate on the expression appearing on the right side of the operand, infix operators operate on the
 * two expressions on the left and right hand (LHS and RHS, respectively) side, and postfix operators operate on the
 * left side of the operand.
 */
sealed class Operator : Expression()
{
    // UNARY OPERATORS
    
    /**
     * Represents the positive value of [expression]. In almost all cases, this operation is a no-op and should not be
     * utilized in software. It exists primarily to support the `+1` syntax, for symmetrical purposes (as `-1` is legal
     * syntax).
     */
    data class UnaryPlus(val expression: Expression) : Operator()
    
    /**
     * Represents the negative value of [expression].
     */
    data class UnaryMinus(val expression: Expression) : Operator()
    
    // COMPARISON OPERATORS
    
    /**
     * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
     * [lhs] value is considered less than [rhs].
     */
    data class Less(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
     * [lhs] value is considered less than, or equal to, [rhs].
     */
    data class LessEqual(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
     * [lhs] value is considered greater than [rhs].
     */
    data class Greater(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
     * [lhs] value is considered greater than, or equal to, [rhs].
     */
    data class GreaterEqual(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
     * [lhs] value is considered equal to [rhs].
     */
    data class Equal(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
     * [lhs] value is considered not equal to [rhs].
     */
    data class NotEqual(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a three-way comparison between [lhs] and [rhs].
     *
     * TODO: Describe what this operator *actually* does, and how to make sense of what it does...
     */
    data class ThreeWay(val lhs: Expression, val rhs: Expression) : Operator()
    
    // LOGICAL OPERATORS
    
    /**
     * Performs a logical `and` operation between the [lhs] and [rhs] expressions. Typically, this operator is used to
     * determine whether both expressions evaluate to `true`.
     */
    data class And(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a logical `or` operation between the [lhs] and [rhs] expressions. Typically, this operator is used to
     * determine whether either expressions evaluate to `true`.
     */
    data class Or(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a logical `xor` operation between the [lhs] and [rhs] expressions. Typically, this operator is used to
     * determine whether one *or* the other expression evaluate to `true`.
     */
    data class Xor(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a logical `not` operation on the given [expression]. Typically, this operator is used to flip the
     * expression's boolean value.
     */
    data class Not(val expression: Expression) : Operator()
    
    // ARITHMETIC OPERATORS
    
    /**
     * Performs a sum operation between the [lhs] and [rhs] expressions.
     */
    data class Add(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a subtraction operation between the [lhs] and [rhs] expressions.
     */
    data class Subtract(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a multiplication operation between the [lhs] and [rhs] expressions.
     */
    data class Multiply(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a division operation between the [lhs] and [rhs] expressions.
     */
    data class Divide(val lhs: Expression, val rhs: Expression) : Operator()
    
    /**
     * Performs a modulo operation between the [lhs] and [rhs] expressions.
     */
    data class Modulo(val lhs: Expression, val rhs: Expression) : Operator()
}
