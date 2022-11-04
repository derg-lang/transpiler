package com.github.derg.transpiler.ast

import com.github.derg.transpiler.core.Name

/**
 * Encapsulation of data and functionality is achieved through visibility. Certain functionality, behavior, and state
 * may be concealed behind an abstraction, where anything outside the abstraction cannot see the internal behavior or
 * state.
 *
 * Visibilities are hierarchical in nature. Everything which is accessible under the public domain is also accessible by
 * everything which could access protected elements, and similarly for everything which has access to private elements.
 */
enum class Visibility
{
    /**
     * The object is accessible to everything within the same module as the object was declared in. The object will not
     * be visible to anything outside the current module.
     */
    PUBLIC,
    
    /**
     * The object is only accessible to the current type in which the object was declared. For example, a private
     * variable may only be accessed by the object where the variable was declared.
     */
    PRIVATE,
}

/**
 * The kind of variable determines what is permitted regarding the data the variable holds. The kind specifies the
 * variable mutability level, restricting the variable from never-changing, to fully mutable.
 */
enum class Mutability
{
    /**
     * The variable is considered a constant. The value can never change, and the properties associated with the value
     * cannot change either. The variable is considered deeply constant, and the user cannot assume that its memory
     * address will remain constant. All variables which are constant may be inlined, or statically computed to other
     * values at compile time, wherever possible.
     */
    VALUE,
    
    /**
     * The variable is considered unchanging and can never change. However, the variable is still possible to mutate by
     * invoking mutating functions or writing different values to the variable properties.
     */
    VARYING,
    
    /**
     * The variable is considered fully mutable and can change in every imaginable way. The variable may be re-assigned
     * a different value, and mutated by either function calls or by updating the variable properties.
     */
    MUTABLE,
}

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

/**
 * Every source code file is parsed into a single segment, in which the total collection of segments with the same
 * [module] name form a single module. The segment forms the most basic building block when structuring code, and is the
 * component which allows code fragmentation to take place.
 *
 * @param imports The modules which are to be imported into this segment.
 * @param definitions All components which are injected into the module by this segment.
 */
data class Segment(val module: Name?, val imports: Set<Name>, val definitions: List<Definition>)
