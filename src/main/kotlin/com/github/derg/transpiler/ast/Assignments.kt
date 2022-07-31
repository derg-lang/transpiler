package com.github.derg.transpiler.ast

import com.github.derg.transpiler.core.Name

/**
 * Expressions may be turned into l-values by being stored in a variable of any kind. In doing so, the r-value
 * expression may be referenced or otherwise accessed again at any later time. All statements which alters the value of
 * a variable is considered to be an assignment.
 */
sealed class Assignment : Expression()
{
    /**
     * Assigns the given [expression] to [name], returning the new value of [name].
     */
    data class Assign(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Adds the given [expression] to [name], returning the new value of [name].
     */
    data class AssignAdd(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Subtracts the given [expression] from [name], returning the new value of [name].
     */
    data class AssignSubtract(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Multiplies [name] with the given [expression], returning the new value of [name].
     */
    data class AssignMultiply(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Divides [name] by the given [expression], returning the new value of [name].
     */
    data class AssignDivide(val name: Name, val expression: Expression) : Assignment()
    
    /**
     * Assigns the modulo of [name] with the given [expression], returning the new value of [name].
     */
    data class AssignModulo(val name: Name, val expression: Expression) : Assignment()
}
