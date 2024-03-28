package com.github.derg.transpiler.source.hir

/**
 * Executable parts of the program are represented as instructions. Every instruction performs exactly one task,
 * although it may be composed out of multiple calculations.
 */
sealed interface HirInstruction

/**
 * Assigns the specified [value] to the object located under the given [instance].
 */
data class HirAssign(val instance: HirValue, val value: HirValue) : HirInstruction

/**
 * Conditional execution is possible by branching the control flow one a [predicate]. If the predicates matches, the
 * [success] branch is selected, otherwise the [failure] branch is selected.
 */
data class HirBranch(
    val predicate: HirValue,
    val success: List<HirInstruction>,
    val failure: List<HirInstruction>,
) : HirInstruction

/**
 * Evaluates the [expression], and executes any side effects which may arise as a consequence. The [expression] is not
 * permitted to evaluate to any non-void value or error.
 */
data class HirEvaluate(val expression: HirValue) : HirInstruction

/**
 * Exist the current function call, returning control flow to whoever called the function in the first place.
 */
data object HirReturn : HirInstruction

/**
 * Exits the current function call, returning control flow to whoever called the function in the first place. The
 * error value of the function is provided by the given [value].
 */
data class HirReturnError(val value: HirValue) : HirInstruction

/**
 * Exits the current function call, returning control flow to whoever called the function in the first place. The
 * return value of the function is provided by the given [value].
 */
data class HirReturnValue(val value: HirValue) : HirInstruction
