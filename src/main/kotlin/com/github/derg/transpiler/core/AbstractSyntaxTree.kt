package com.github.derg.transpiler.core

/**
 * The type of the variable determines what is permitted using the variable, and what is not. A value variable does not
 * permit any changes, neither to the variable itself, nor can the object be mutated. Varying variables does not allow
 * the variable to change either, although the object may be mutated. A mutable variable may be mutated and re-assigned
 * as much as the user desires.
 */
enum class VariableKind
{
    VAL,
    VAR,
    MUT
}

/**
 *
 */
sealed class Node

/**
 * TODO: Certain source code object may be declared ahead of time, before actually being defined.
 */
sealed class NodeDeclaration : Node()
{
    data class Variable(
        val kind: VariableKind,
        val name: Name,
        val type: Name?,
        val expression: NodeExpression,
    ) : NodeDeclaration()
    
    data class Function(
        val name: Name,
        val type: Name?,
        val parameters: Map<Name, Name>,
    ) : NodeDeclaration()
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
    data class Assign(val variable: Name, val expression: NodeExpression) : NodeAssignment()
    data class AssignPlus(val variable: Name, val expression: NodeExpression) : NodeAssignment()
    data class AssignMinus(val variable: Name, val expression: NodeExpression) : NodeAssignment()
    data class AssignMultiply(val variable: Name, val expression: NodeExpression) : NodeAssignment()
    data class AssignDivide(val variable: Name, val expression: NodeExpression) : NodeAssignment()
    // ... and so on, just a fair number of assignment options will be added here...
}

/**
 * TODO: Certain source code tokens are considered expressions, and are the executable part of the program.
 */
sealed class NodeExpression : Node()
{
    data class Bool(val value: Boolean) : NodeExpression()
    data class Numeric(val value: Number, val type: Name?) : NodeExpression()
    data class Textual(val value: String, val type: Name?) : NodeExpression()
    
    data class Variable(val name: Name) : NodeExpression()
    data class Function(val name: Name, val parameters: List<ParameterNode>) : NodeExpression()
    
    data class IncrementPost(val name: Name) : NodeExpression()
    data class IncrementPre(val name: Name) : NodeExpression()
    data class DecrementPost(val name: Name) : NodeExpression()
    data class DecrementPre(val name: Name) : NodeExpression()
    
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

data class ParameterNode(val name: Name?, val expression: NodeExpression)
