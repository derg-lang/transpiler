package com.github.derg.transpiler.source.thir

/**
 * Executable parts of the program are represented as instructions. Every instruction performs exactly one task,
 * although it may be composed out of multiple calculations.
 */
sealed interface ThirInstruction

/**
 * Assigns the specified [value] to the [variable].
 */
data class ThirAssign(val variable: ThirVariable, val value: ThirValue) : ThirInstruction

/**
 * Conditional execution is possible by branching the control flow one a [predicate]. If the predicates matches, the
 * [success] branch is selected, otherwise [failure] is.
 */
data class ThirCondition(val predicate: ThirValue, val success: ThirScope, val failure: ThirScope) : ThirInstruction

/**
 * The null operation, does nothing. It will always be removed from the instruction set. It is used to handle various
 * special cases where statements contributing to the structure of the program occur. Such statements do not include any
 * executable code, and thus will be omitted.
 */
object ThirNop : ThirInstruction

/**
 * Exits the current function call, returning control flow to whomever called the function in the first place. No
 * return value is provided by the function.
 */
object ThirReturn : ThirInstruction

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
