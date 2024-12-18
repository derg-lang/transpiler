package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*

/**
 * All expressions are granted exactly one type of all possibilities. Certain symbols within the codebase, such as
 * variables, parameters, fields, functions, and so on have their own types. The type information can be used to verify
 * that the program is well-typed, avoiding errors such as passing the wrong type as a function parameter.
 */
sealed interface HirType
{
    /**
     * The function type describes a function-like object. These types indicate that the object may be used as a
     * callable object, enabling it to be invoked as a function. Note that this only encodes the type information, not
     * anything about the value itself.
     *
     * Example syntax: `fun(Bool, Int32): Error -> Int32`
     *
     * @param value The type which is permitted to be returned as a value.
     * @param error The type which is permitted to be returned as an error.
     * @param parameters The types of all the runtime parameters the function accepts.
     */
    data class Function(val value: HirType?, val error: HirType?, val parameters: List<HirParameterDynamic>) : HirType
    
    /**
     * The structure represents a specific layout of data in memory. This type is used to represent a specific data
     * structure which has been declared within the code. Data structures are basic containers of data.
     *
     * Example syntax: `mut List[Int32]`
     *
     * @param name The name of the structure.
     * @param mutability The type of mutation which is permitted on the type.
     * @param parameters The types of all the compile-time parameters the structure accepts.
     */
    data class Structure(val name: String, val mutability: Mutability, val parameters: List<HirParameterStatic>) : HirType
    
    /**
     * The union type describes a collection of types. This type encodes the meaning that a value must be exactly one of
     * a number of possible types.
     *
     * Example syntax: `Bool | Int32`
     *
     * @param types The types which a value is permitted to take, the value must be exactly one of them.
     */
    data class Union(val types: Set<HirType>) : HirType
}

/**
 * The static parameter type describes something which can be accepted to customize other objects such as data
 * structures, functions, unions, and any other customizable object. These types are used to determine which
 * compile-time parameters may be passed to other objects.
 */
sealed interface HirParameterStatic
{
    /**
     * A specific type which must be provided by the user. The type can be used to generalize which types a function or
     * a data structure can work with, i.e. the element a list can hold.
     *
     * Example syntax: `name`
     *
     * @param name The name of the parameter.
     */
    data class Type(val name: String) : HirParameterStatic
    
    /**
     * A specific value which must be provided by the user. The value can be used to generalize some property of a
     * function or a data structure, i.e. the length of an array.
     *
     * Example syntax: `name: Int32`
     *
     * @param name The name of the parameter.
     * @param type The type the compile-time value must be adhering to.
     */
    data class Value(val name: String, val type: HirType) : HirParameterStatic
}

/**
 * The dynamic parameter type describes something which can be accepted when invoking a function. These types are used
 * to determine which expressions can be legally passed as parameters to any callable.
 *
 * Example syntax: `move name: Int32`
 *
 * @param name The name of the parameter.
 * @param type The type of possible values which can be used as the parameter.
 * @param passability The method used to pass the parameter into the function.
 */
data class HirParameterDynamic(val name: String, val type: HirType, val passability: Passability)
