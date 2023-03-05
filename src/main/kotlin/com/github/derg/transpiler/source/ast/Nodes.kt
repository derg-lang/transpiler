package com.github.derg.transpiler.source.ast

/**
 * Every single element in the source code may be represented as a node in the abstract syntax tree. The nodes do not
 * have to resemble the source code in any way, shape, or form. Each node is produced from the source code via the
 * parser.
 *
 * The collection of nodes and the relation between the nodes fully describes the source code which has been parsed. The
 * ordering of the nodes describes the order in which the program should be executed, and the initialization order of
 * every element within the source code.
 */
sealed interface Node

/**
 * Expressions are computable bits of code which resolves down to a single value and type. These code elements cannot be
 * re-assigned to other values, and do not occupy any space in memory. Intermediary computations may be stored on the
 * stack, although the final value will either be used as a parameter for a procedure call, or stored in a variable.
 */
sealed interface Expression : Node

/**
 * Statements are executable bits of code, which either performs an operation with a side effect, or determines the
 * control flow. Examples include assigning a value to a variable and returning from a sub-routine, respectively. Note
 * that expressions are not statements.
 */
sealed interface Statement : Node
