package com.github.derg.transpiler.core

import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.ast.*

/**
 * Pretty-prints the entire abstract syntax tree defined from [this] node.
 */
fun Segment.print() = println(prettify())

/**
 * Converts [this] node into a string representation of the tree. Child nodes are included in the string on new lines
 * where applicable, with the indentation levels incremented where applicable.
 */
private fun Node.prettify(): String = when (this)
{
    is Access.Function       -> prettify()
    is Access.Subscript      -> prettify()
    is Access.Variable       -> name
    is Assignment.Assign     -> stringifyAssignment(name, expression)
    is Constant.Bool         -> stringifyLiteral(value, null)
    is Constant.Real         -> stringifyLiteral(value, literal)
    is Constant.Text         -> stringifyLiteral(value, literal)
    is Control.Branch        -> prettify()
    is Control.Invoke        -> stringifyStatement("CALL", expression)
    is Control.Raise         -> stringifyStatement("RAISE", expression)
    is Control.Return        -> stringifyStatement("RETURN", expression)
    is Definition.Function   -> prettify()
    is Definition.Type       -> prettify()
    is Definition.Variable   -> prettify()
    is Operator.Add          -> stringifyBinary("+", lhs, rhs)
    is Operator.And          -> stringifyBinary("&&", lhs, rhs)
    is Operator.Catch        -> stringifyBinary(":", lhs, rhs)
    is Operator.Divide       -> stringifyBinary("/", lhs, rhs)
    is Operator.Equal        -> stringifyBinary("==", lhs, rhs)
    is Operator.Greater      -> stringifyBinary(">", lhs, rhs)
    is Operator.GreaterEqual -> stringifyBinary(">=", lhs, rhs)
    is Operator.Less         -> stringifyBinary("<", lhs, rhs)
    is Operator.LessEqual    -> stringifyBinary("<=", lhs, rhs)
    is Operator.Minus        -> stringifyUnary("-", expression)
    is Operator.Modulo       -> stringifyBinary("%", lhs, rhs)
    is Operator.Multiply     -> stringifyBinary("*", lhs, rhs)
    is Operator.Not          -> stringifyUnary("!", expression)
    is Operator.NotEqual     -> stringifyBinary("!=", lhs, rhs)
    is Operator.Or           -> stringifyBinary("||", lhs, rhs)
    is Operator.Plus         -> stringifyUnary("+", expression)
    is Operator.Raise        -> stringifyBinary("!:", lhs, rhs)
    is Operator.Subtract     -> stringifyBinary("-", lhs, rhs)
    is Operator.ThreeWay     -> stringifyBinary("<=>", lhs, rhs)
    is Operator.Xor          -> stringifyBinary("^^", lhs, rhs)
    is When                  -> prettify()
}

/**
 * Indents all lines within the string by one more level.
 */
private fun String.indent(): String =
    "\t$this".replace("\n", "\n\t")

/**
 * Formats [this] string using the given [transformation] function. If the string is null, a blank string is returned.
 */
private fun <T> T?.format(transformation: (T) -> String): String =
    if (this == null) "" else transformation(this)

// ...

/**
 * Converts the prefix unary operator into a string representing the combined expression from the given [operator] and
 * [rhs] expression.
 */
private fun stringifyUnary(operator: String, rhs: Expression): String =
    "$operator${rhs.prettify()}"

/**
 * Converts the infix binary operator into a string representing the combined expression from the given [operator] and
 * [lhs] and [rhs] expressions.
 */
private fun stringifyBinary(operator: String, lhs: Expression, rhs: Expression): String =
    "(${lhs.prettify()} $operator ${rhs.prettify()})"

/**
 * Converts the given assignment operation into a string representing an assignment to [name] with the value from the
 * given [expression].
 */
private fun stringifyAssignment(name: Name, expression: Expression): String =
    "ASSIGN $name = ${expression.prettify()}"

/**
 * Converts the given [value] literal into a string representing the literal with the given [type].
 */
private fun stringifyLiteral(value: Any, type: Name?): String =
    value.toString() + type.format { " $it" }

/**
 * Converts the given [statement] instruction into a string representing the statement with the given [expression].
 */
private fun stringifyStatement(statement: String, expression: Expression?): String =
    "$statement ${expression.format { it.prettify() }}"

/**
 * Joins [this] collection of nodes into a single string. If [newlines] are assigned, all elements in [this] collection
 * are placed on a new line. If [indented] is also assigned, all entries are indented one additional level.
 */
private fun <T> Iterable<T>.prettify(newlines: Boolean, indented: Boolean, transformation: (T) -> String): String =
    joinToString(if (newlines) "\n" else ", ") { transformation(it) }.let { if (indented) it.indent() else it }

// ...

private fun Access.Function.prettify(): String =
    "$name(${arguments.joinToString { it.prettify() }})"

private fun Access.Subscript.prettify(): String =
    "$name[${arguments.joinToString { it.prettify() }}]"

private fun Control.Branch.prettify(): String =
    "IF ${predicate.prettify()}\n${success.prettify()}${failure.format { "\nELSE\n${it.prettify()}" }}"

private fun Definition.Function.prettify(): String =
    "$visibility FUN $name(" + parameters.joinToString { it.prettify() } + ")\n{\n" + statements.prettify(newlines = true, indented = true) { it.prettify() } + "\n}"

private fun Definition.Type.prettify(): String =
    "$visibility TYPE $name\n{\n" + properties.prettify(newlines = true, indented = true) { it.prettify() } + "\n}"

private fun Definition.Variable.prettify(): String =
    "$visibility $mutability $name = ${value.prettify()}"

private fun When.prettify(): String =
    "WHEN ${expression.prettify()}\n" + branches.joinToString("\n") { "${it.first.prettify()} -> ${it.second.prettify()}".indent() } + default.format { "\nELSE -> ${it.prettify()}".indent() }

// ...

private fun Argument.prettify(): String =
    "${name.format { "$it = " }}${expression.prettify()}"

private fun Property.prettify(): String =
    "$visibility $mutability $name${type.format { ": $it" }}${value.format { " = ${it.prettify()}" }}"

private fun Parameter.prettify(): String =
    "$mutability " + type.format { "$it " } + "$name${value.format { " = $it" }}"

private fun Scope.prettify(): String =
    "{\n" + statements.prettify(newlines = true, indented = true) { it.prettify() } + "\n}"

private fun Segment.prettify(): String = """
    MODULE ${module ?: "<none>"}
    IMPORT ${imports.joinToString().ifBlank { "<none>" }}
""".trimIndent() + "\n" + definitions.prettify(newlines = true, indented = false) { it.prettify() }
