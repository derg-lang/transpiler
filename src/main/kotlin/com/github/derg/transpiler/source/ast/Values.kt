package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.*
import java.math.*

/**
 * Expressions are computable bits of code which resolves down to a single value and type. These code elements cannot be
 * re-assigned to other values, and do not occupy any space in memory. Intermediary computations may be stored on the
 * stack, although the final value will either be used as a parameter for a procedure call, or stored in a variable.
 */
sealed interface AstValue

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Access
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * All variables are permitted to hold a value, which may be accessed by examining the variable itself. Variables are
 * given a unique [name] within every scope, allowing the analyzer to resolve which variable is being referenced. Any
 * type property will be represented as a variable in the abstract syntax tree.
 *
 * The symbol which is loaded can be specialized with any number of [template arguments][temArgs]. These parameters are
 * specified at compile-time, and parameterizes the loaded object.
 */
data class AstLoad(val name: String, val temArgs: List<AstArgument>) : AstValue

/**
 * All procedure calls may either refer to a function with the specified [name] being invoked, or the function operator
 * being invoked on an instance with the given [name]. Every procedure call is permitted an arbitrary number of
 * [value arguments][valArgs] and [template arguments][temArgs].
 */
data class AstCall(val name: String, val temArgs: List<AstArgument>, val valArgs: List<AstArgument>) : AstValue

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Represents a `true` or `false` [value]. Booleans cannot hold any other value, and cannot be given a user-defined
 * literal.
 */
data class AstBool(val value: Boolean) : AstValue

/**
 * Any real integral value may be represented in [value]. The precision of the number is limited by the target language
 * and hardware. All integers may have an optional [literal] associated with them, indicating which user-defined literal
 * should be used to interpret the integer.
 */
data class AstInteger(val value: BigInteger, val literal: String) : AstValue

/**
 * Any real numeric value may be represented in [value]. The precision of the number is limited by the target language
 * and hardware. All numbers may have an optional [literal] associated with them, indicating which user-defined literal
 * should be used to interpret the number.
 */
data class AstDecimal(val value: BigDecimal, val literal: String) : AstValue

/**
 * All strings may be represented in [value]. All types of text are permitted, although limitations may be imposed by
 * the target language and hardware. All strings may have an optional [literal] associated with them, indicating which
 * user-defined literal should be used to interpret the value.
 */
data class AstText(val value: String, val literal: String) : AstValue

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Operators
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// UNARY OPERATORS

/**
 * Represents the positive value of [expression]. In all cases, this operation is a no-op. It exists to support the `+1`
 * syntax, for symmetrical purposes (as `-1` is legal syntax).
 */
data class AstPlus(val expression: AstValue) : AstValue

/**
 * Represents the negative value of [expression].
 */
data class AstMinus(val expression: AstValue) : AstValue

/**
 * Represents the inverse value of [expression]. Typically, this operator is used to flip the expression's boolean
 * value.
 */
data class AstNot(val expression: AstValue) : AstValue

// COMPARISON OPERATORS

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered less than [rhs].
 */
data class AstLess(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered less than, or equal to, [rhs].
 */
data class AstLessEqual(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered greater than [rhs].
 */
data class AstGreater(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered greater than, or equal to, [rhs].
 */
data class AstGreaterEqual(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered equal to [rhs].
 */
data class AstEqual(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a comparison between [lhs] and [rhs]. Commonly, this operation will result a `true` result whenever the
 * [lhs] value is considered not equal to [rhs].
 */
data class AstNotEqual(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a three-way comparison between [lhs] and [rhs].
 *
 * TODO: Describe what this operator *actually* does, and how to make sense of what it does...
 */
data class AstThreeWay(val lhs: AstValue, val rhs: AstValue) : AstValue

// LOGICAL OPERATORS

/**
 * Performs a logical `and` operation between the [lhs] and [rhs] expressions. Typically, this operator is used to
 * determine whether both expressions evaluate to `true`.
 */
data class AstAnd(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a logical `or` operation between the [lhs] and [rhs] expressions. Typically, this operator is used to
 * determine whether either expressions evaluate to `true`.
 */
data class AstOr(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a logical `xor` operation between the [lhs] and [rhs] expressions. Typically, this operator is used to
 * determine whether one *or* the other expression evaluate to `true`.
 */
data class AstXor(val lhs: AstValue, val rhs: AstValue) : AstValue

// ARITHMETIC OPERATORS

/**
 * Performs a sum operation between the [lhs] and [rhs] expressions.
 */
data class AstAdd(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a subtraction operation between the [lhs] and [rhs] expressions.
 */
data class AstSubtract(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a multiplication operation between the [lhs] and [rhs] expressions.
 */
data class AstMultiply(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a division operation between the [lhs] and [rhs] expressions.
 */
data class AstDivide(val lhs: AstValue, val rhs: AstValue) : AstValue

/**
 * Performs a modulo operation between the [lhs] and [rhs] expressions.
 */
data class AstModulo(val lhs: AstValue, val rhs: AstValue) : AstValue

// ERROR OPERATORS

data class AstCatch(val lhs: AstValue, val rhs: AstValue, val capture: Capture) : AstValue

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Unsorted
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Specifies a conditional branching on the given [expression], where exactly one of the [branches] are chosen. If none
 * of the conditions among all branches evaluate to true, the [default] expression is selected instead. The default
 * expression may only be omitted when all possible conditions are provably covered by the compiler.
 */
data class AstWhen(
    val expression: AstValue,
    val branches: List<Pair<AstValue, AstValue>>,
    val default: AstValue?,
) : AstValue
