package com.github.derg.transpiler.source.ast

import java.math.*

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Access
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * All variables are permitted to hold a value, which may be accessed by examining the variable itself. Variables are
 * given a unique [name] within every scope, allowing the analyzer to resolve which variable is being referenced. Any
 * type property will be represented as a variable in the abstract syntax tree.
 */
data class AstRead(val name: String) : AstAccess

/**
 * All procedure calls may either refer to a function with the specified [name] being invoked, or the function operator
 * being invoked on an instance with the given [name]. Every procedure call is permitted an arbitrary number of
 * [value arguments][valArgs] and [template arguments][temArgs].
 */
data class AstCall(val name: String, val temArgs: List<AstArgument>, val valArgs: List<AstArgument>) : AstAccess

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Represents a `true` or `false` [value]. Booleans cannot hold any other value, and cannot be given a user-defined
 * literal.
 */
data class AstBool(val value: Boolean) : AstConstant

/**
 * Any real integral value may be represented in [value]. The precision of the number is limited by the target language
 * and hardware. All integers may have an optional [literal] associated with them, indicating which user-defined literal
 * should be used to interpret the integer.
 */
data class AstInteger(val value: BigInteger, val literal: String) : AstConstant

/**
 * Any real numeric value may be represented in [value]. The precision of the number is limited by the target language
 * and hardware. All numbers may have an optional [literal] associated with them, indicating which user-defined literal
 * should be used to interpret the number.
 */
data class AstDecimal(val value: BigDecimal, val literal: String) : AstConstant

/**
 * All strings may be represented in [value]. All types of text are permitted, although limitations may be imposed by
 * the target language and hardware. All strings may have an optional [literal] associated with them, indicating which
 * user-defined literal should be used to interpret the value.
 */
data class AstText(val value: String, val literal: String) : AstConstant

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Operators
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// UNARY OPERATORS

/**
 * Represents the positive value of [expression]. In all cases, this operation is a no-op. It exists to support the `+1`
 * syntax, for symmetrical purposes (as `-1` is legal syntax).
 */
data class AstPlus(val expression: AstExpression) : AstOperator

/**
 * Represents the negative value of [expression].
 */
data class AstMinus(val expression: AstExpression) : AstOperator

/**
 * Represents the inverse value of [expression]. Typically, this operator is used to flip the expression's boolean
 * value.
 */
data class AstNot(val expression: AstExpression) : AstOperator

// COMPARISON OPERATORS

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered less than [rhs].
 */
data class AstLess(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered less than, or equal to, [rhs].
 */
data class AstLessEqual(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered greater than [rhs].
 */
data class AstGreater(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered greater than, or equal to, [rhs].
 */
data class AstGreaterEqual(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered equal to [rhs].
 */
data class AstEqual(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered not equal to [rhs].
 */
data class AstNotEqual(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a three-way comparison between [lhs] and [rhs].
 *
 * TODO: Describe what this operator *actually* does, and how to make sense of what it does...
 */
data class AstThreeWay(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

// LOGICAL OPERATORS

/**
 * Performs a logical `and` operation between the [lhs] and [rhs] expressions. Typically, this operator is used to
 * determine whether both expressions evaluate to `true`.
 */
data class AstAnd(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a logical `or` operation between the [lhs] and [rhs] expressions. Typically, this operator is used to
 * determine whether either expressions evaluate to `true`.
 */
data class AstOr(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a logical `xor` operation between the [lhs] and [rhs] expressions. Typically, this operator is used to
 * determine whether one *or* the other expression evaluate to `true`.
 */
data class AstXor(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

// ARITHMETIC OPERATORS

/**
 * Performs a sum operation between the [lhs] and [rhs] expressions.
 */
data class AstAdd(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a subtraction operation between the [lhs] and [rhs] expressions.
 */
data class AstSubtract(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a multiplication operation between the [lhs] and [rhs] expressions.
 */
data class AstMultiply(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a division operation between the [lhs] and [rhs] expressions.
 */
data class AstDivide(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Performs a modulo operation between the [lhs] and [rhs] expressions.
 */
data class AstModulo(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

// ERROR OPERATORS

/**
 * Catches any errors which are raised in the [lhs] expression, replacing errors with the [rhs] expression. In essence,
 * this operator allows errors to be caught and replaced with a default value in their place. That is, if an error
 * occurs in the base expression, the error is replaced with the value produced by the second expression. When no error
 * occurs, the outcome of the expression is [lhs], otherwise it is [rhs].
 */
data class AstCatch(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Catches any errors which are raised in the [lhs] expression, and raises a new error based on the value calculated in
 * the [rhs] expression. The original error may be referenced by the second expression, allowing the original error to
 * be transformed into a different error.
 */
data class AstRaise(val lhs: AstExpression, val rhs: AstExpression) : AstOperator

/**
 * Catches any errors which are raised in the [lhs] expression, and returns a new value based on the value calculated in
 * the [rhs] expression. The original error may be referenced by the second expression, allowing the original error to
 * be transformed into a different value.
 */
// TODO: Add return expression
// data class AstReturn(val lhs: Expression, val rhs: Expression) : Operator

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Unsorted
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Specifies a conditional branching on the given [expression], where exactly one of the [branches] are chosen. If none
 * of the conditions among all branches evaluate to true, the [default] expression is selected instead. The default
 * expression may only be omitted when all possible conditions are provably covered by the compiler.
 */
data class AstWhen(
    val expression: AstExpression,
    val branches: List<Pair<AstExpression, AstExpression>>,
    val default: AstExpression?,
) : AstExpression
