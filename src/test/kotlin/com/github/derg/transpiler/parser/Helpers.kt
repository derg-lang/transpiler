package com.github.derg.transpiler.parser

import com.github.derg.transpiler.ast.Access
import com.github.derg.transpiler.ast.Value
import com.github.derg.transpiler.lexer.tokenize
import com.github.derg.transpiler.util.Result

/**
 * Generates a legal context from the input [source] code.
 */
internal fun contextOf(source: String): Context = Context(tokenize(source).map { it.data })

/**
 * Attempts to parse the [source] code using the current pattern.
 */
internal fun <Type> Pattern<Type>.parse(source: String): Result<Type, String> = parse(contextOf(source))

internal val Boolean.value: Value.Bool get() = Value.Bool(this)
internal val Int.value: Value.Real get() = Value.Real(toBigDecimal(), null)
internal val String.variable: Access.Variable get() = Access.Variable(this)
