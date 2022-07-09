package com.github.derg.transpiler.core

/**
 *
 */
sealed class Node

/**
 * TODO: Certain source code object may be declared ahead of time, before actually being defined.
 */
sealed class NodeDeclaration : Node()
{
    data class Variable(val name: Name, val type: Name) : NodeDeclaration()
    data class Function(val name: Name, val type: Name, val parameters: Map<Name, Name>) : NodeDeclaration()
}

/**
 * TODO: Certain source code tokens are considered flow control, and are the executable part of the program.
 */
sealed class NodeControlFlow : Node()
{
    object Break : NodeControlFlow()
    data class Return(val expression: NodeExpression) : NodeControlFlow()
    // ... and so on, just a CRAPTON of control flow will be added here...
}

/**
 * TODO: Certain source code tokens are considered assignments, and are part of the executable part of the program.
 */
sealed class NodeAssignment : Node()
{
    data class Assign(val variable: Name, val expression: NodeExpression) : NodeExpression()
    data class AssignPlus(val variable: Name, val expression: NodeExpression) : NodeExpression()
    data class AssignMinus(val variable: Name, val expression: NodeExpression) : NodeExpression()
    data class AssignMultiply(val variable: Name, val expression: NodeExpression) : NodeExpression()
    data class AssignDivide(val variable: Name, val expression: NodeExpression) : NodeExpression()
    // ... and so on, just a fair number of assignment options will be added here...
}

/**
 * TODO: Certain source code tokens are considered expressions, and are the executable part of the program.
 */
sealed class NodeExpression : Node()
{
    data class Bool(val value: Boolean) : NodeExpression()
    data class Numeric(val value: Number) : NodeExpression()
    data class Textual(val value: String) : NodeExpression()
    
    data class IncrementPost(val variable: Name) : NodeExpression()
    data class IncrementPre(val variable: Name) : NodeExpression()
    data class DecrementPost(val variable: Name) : NodeExpression()
    data class DecrementPre(val variable: Name) : NodeExpression()
    
    data class Unary(val expression: NodeExpression) : NodeExpression()
    data class Plus(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class Minus(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class Multiply(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class Divide(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    
    data class Less(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class LessEqual(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class Greater(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class GreaterEqual(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class Equal(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class NotEqual(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class ThreeWay(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    
    data class LogicalAnd(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class LogicalOr(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class LogicalXor(val lhs: NodeExpression, val rhs: NodeExpression) : NodeExpression()
    data class LogicalNot(val expression: NodeExpression) : NodeExpression()
    // ... and so on, just a CRAPTON of expressions will be added here...
}
