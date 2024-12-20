package com.github.derg.transpiler.source.ast

/**
 * Statements are executable bits of code, which either performs an operation with a side effect, or determines the
 * control flow. Examples include assigning a value to a variable and returning from a sub-routine, respectively. Note
 * that expressions are not statements.
 */
sealed interface AstInstruction

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Assignment
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Assigns the given [expression] to [name], returning the new value of [name].
 */
data class AstAssign(val name: String, val expression: AstValue) : AstInstruction
data class AstAssignAdd(val name: String, val expression: AstValue) : AstInstruction
data class AstAssignDivide(val name: String, val expression: AstValue) : AstInstruction
data class AstAssignModulo(val name: String, val expression: AstValue) : AstInstruction
data class AstAssignMultiply(val name: String, val expression: AstValue) : AstInstruction
data class AstAssignSubtract(val name: String, val expression: AstValue) : AstInstruction

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Control flow
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Splits the execution based on the [predicate]. If the predicate returns a `true` value, the [success] path is chosen,
 * otherwise the [failure] path is chosen (if specified).
 */
data class AstBranch(val predicate: AstValue, val success: List<AstInstruction>, val failure: List<AstInstruction>) : AstInstruction

/**
 * While ordinarily expressions are not permitted to be statements, functions may be invoked directly as statements
 * under certain circumstances. The [expression] is not permitted to resolve down to any value, it must resolve to
 * something which does not have a type, and does not raise any error.
 */
data class AstEvaluate(val expression: AstValue) : AstInstruction

/**
 * Specifies that the execution flow should exit the current function, returning control flow back to the caller. Note
 * that the function cannot be exited in this manner if the function expects a return value.
 */
data object AstReturn : AstInstruction

/**
 * Specifies that a function should raise a specific [expression]. The raise statement functions similarly to the return
 * statement, in that execution is resumed to the caller of the function. When a value is raised from a function, this
 * usually indicates that the function has failed to uphold its contract and produced a value which does not conform to
 * the expectations of the caller.
 */
data class AstReturnError(val expression: AstValue) : AstInstruction

/**
 * Specifies that a function should return a specific [expression]. The return statement marks the point where the
 * execution is resumed to the caller of the function, executing nothing else in the body of the function. Returning a
 * value from a function indicates usually that the function has succeeded in producing a usable value.
 */
data class AstReturnValue(val expression: AstValue) : AstInstruction
