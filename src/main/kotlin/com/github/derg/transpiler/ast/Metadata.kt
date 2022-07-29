package com.github.derg.transpiler.ast

import com.github.derg.transpiler.core.Name

/**
 * Function parameters consists of a single [expression] which specifies which value they have, and optionally the
 * [name] of the parameters the [expression] is associated with. Parameters must be specified in the correct order,
 * although the ordering may be ignored if the parameters are named instead.
 */
data class Parameter(val name: Name?, val expression: Expression)
