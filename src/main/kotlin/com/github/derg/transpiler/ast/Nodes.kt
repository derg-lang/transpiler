package com.github.derg.transpiler.ast

/**
 * Every single element in the source code may be represented as a node in the abstract syntax tree. The nodes do not
 * have to resemble the source code in any way, shape, or form. Each node is produced from the source code via the
 * parser.
 *
 * The collection of nodes and the relation between the nodes fully describes the source code which has been parsed. The
 * ordering of the nodes describes the order in which the program should be executed, and the initialization order of
 * every element within the source code.
 */
sealed class Node

/**
 * While typically structures in the source code may be represented by a single node, sequences of nodes are used to
 * describe larger components such as modules, namespaces, scopes, and so on.
 */
typealias Sequence = List<Node>

/**
 * Structural components within the source code must be defined to describe their behavior fully. While certain objects
 * such as functions may be declared before being defined, all such objects must be defined before being used. The
 * definition of the object varies from object to object.
 */
sealed class Definition : Node()

/**
 * Expressions are computable bits of code which resolves down to a single value and type. These code elements cannot be
 * re-assigned to other values, and do not occupy any space in memory. Intermediary computations may be stored on the
 * stack, although the final value will either be used as a parameter for a procedure call, or stored in a variable.
 */
sealed class Expression : Node()
