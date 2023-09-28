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
sealed interface AstNode

/**
 * Retrieving values from existing objects in source code is known as accessing the value. Various methods for accessing
 * data are viable, such as reading the value directly from a variable, calculating the value of a function call, or
 * similar methods.
 */
sealed interface AstAccess : AstExpression

/**
 * Expressions may be turned into l-values by being stored in a variable of any kind. In doing so, the r-value
 * expression may be referenced or otherwise accessed again at any later time. All statements which alters the value of
 * a variable is considered to be an assignment.
 */
sealed interface AstAssignment : AstStatement

/**
 * The flow of the program is determined by the control structures. All control structures allows execution to either
 * break out of the current function, enter another function, branch, loop, and anything else the programmer may desire.
 */
sealed interface AstControl : AstStatement

/**
 * Any constant in source code, either a number, a string, or a boolean value, are treated specially. Any constant value
 * has the property of never changing, and as such allows optimizations such as constant folding, inlining, and compile
 * time calculations.
 */
sealed interface AstConstant : AstExpression

/**
 * Structural components within the source code must be defined to describe their behavior fully. While certain objects
 * such as functions may be declared before being defined, all such objects must be defined before being used. The
 * definition of the object varies from object to object.
 */
sealed interface AstDefinition : AstNode
{
    /**
     * The name of the symbol.
     */
    val name: String
}

/**
 * Expressions are computable bits of code which resolves down to a single value and type. These code elements cannot be
 * re-assigned to other values, and do not occupy any space in memory. Intermediary computations may be stored on the
 * stack, although the final value will either be used as a parameter for a procedure call, or stored in a variable.
 */
sealed interface AstExpression : AstNode

/**
 * Any two expressions may be combined with an operator to form a new value. The operator determines which operation
 * will be performed on the expressions participating in the operation. Certain operations require only one expression,
 * some require two.
 *
 * Prefix operators operate on the expression appearing on the right side of the operand, infix operators operate on the
 * two expressions on the left and right hand (LHS and RHS, respectively) side, and postfix operators operate on the
 * left side of the operand.
 */
sealed interface AstOperator : AstExpression

/**
 * Statements are executable bits of code, which either performs an operation with a side effect, or determines the
 * control flow. Examples include assigning a value to a variable and returning from a sub-routine, respectively. Note
 * that expressions are not statements.
 */
sealed interface AstStatement : AstNode
