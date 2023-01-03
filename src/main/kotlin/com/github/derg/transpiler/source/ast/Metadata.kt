package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.Name

/**
 * Function parameters consists of a single [expression] which specifies which value they have, and optionally the
 * [name] of the parameters the [expression] is associated with. Parameters must be specified in the correct order,
 * although the ordering may be ignored if the parameters are named instead.
 */
data class Parameter(val name: Name?, val expression: Expression)

/**
 * The collection of any number of [statements] within a logical unit is known as a scope. The scope permits arbitrary
 * code to be performed, including defining new variables, functions, types, and so on. All executable parts of the
 * program must be located within a scope.
 *
 * @property isBraced Whether the scope is surrounded with braces or not.
 */
data class Scope(val isBraced: Boolean, val statements: List<Statement>)
