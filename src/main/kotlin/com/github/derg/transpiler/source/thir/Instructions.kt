package com.github.derg.transpiler.source.thir

/**
 * Executable parts of the program are represented as instructions. Every instruction performs exactly one task,
 * although it may be composed out of multiple calculations.
 */
sealed interface Instruction

/**
 * Assigns the specified [value] to the [variable].
 */
data class Assign(val variable: Variable, val value: Value) : Instruction

/**
 * Conditional execution is possible by branching the control flow one a [predicate]. If the predicates matches, the
 * [success] branch is selected, otherwise [failure] is.
 */
data class Condition(val predicate: Value, val success: Scope, val failure: Scope) : Instruction

/**
 * Exits the current function call, returning control flow to whomever called the function in the first place. No
 * return value is provided by the function.
 */
object Exit : Instruction

/**
 * The null operation, does nothing. It will always be removed from the instruction set. It is used to handle various
 * special cases where statements contributing to the structure of the program occur. Such statements do not include any
 * executable code, and thus will be omitted.
 */
object Nop : Instruction

/**
 * Exits the current function call, returning control flow to whoever called the function in the first place. The
 * return value of the function is provided by the given [value].
 */
data class Return(val value: Value) : Instruction

/**
 * Exits the current function call, returning control flow to whoever called the function in the first place. The
 * error value of the function is provided by the given [error].
 */
data class Raise(val error: Value) : Instruction
