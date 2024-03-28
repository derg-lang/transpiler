package com.github.derg.transpiler.source.thir

import java.util.*

/**
 * Executable parts of the program are represented as instructions. Every instruction performs exactly one task,
 * although it may be composed out of multiple calculations.
 */
sealed interface ThirInstruction

/**
 * Assigns the specified [value] to the [symbolId].
 */
data class ThirAssign(val symbolId: UUID, val value: ThirValue) : ThirInstruction

/**
 * Conditional execution is possible by branching the control flow one a [predicate]. If the predicates matches, the
 * [success] branch is selected, otherwise [failure] is.
 */
data class ThirBranch(
    val predicate: ThirValue,
    val success: List<ThirInstruction>,
    val failure: List<ThirInstruction>,
) : ThirInstruction

/**
 * Evaluates the [expression], executes any side effects which may arise as a consequence. The value and error returned from
 * the invocation must resolve to void.
 */
data class ThirEvaluate(val expression: ThirValue) : ThirInstruction

/**
 * Exist the current function call, returning control flow to whoever called the function in the first place.
 */
data object ThirReturn : ThirInstruction

/**
 * Exits the current function call, returning control flow to whoever called the function in the first place. The
 * error value of the function is provided by the given [error].
 */
data class ThirReturnError(val error: ThirValue) : ThirInstruction

/**
 * Exits the current function call, returning control flow to whoever called the function in the first place. The
 * return value of the function is provided by the given [value].
 */
data class ThirReturnValue(val value: ThirValue) : ThirInstruction
