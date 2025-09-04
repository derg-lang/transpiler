package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.*

/**
 * All expressions are granted exactly one type of all possibilities. Certain symbols within the codebase, such as
 * variables, parameters, fields, functions, and so on have their own types. The type information can be used to verify
 * that the program is well-typed, avoiding errors such as passing the wrong type as a function parameter.
 */
sealed interface AstType
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
    data class Function(val value: AstType?, val error: AstType?, val parameters: List<AstParameterDynamic>) : AstType
    
    /**
     * The variable type represents a specific layout of data in memory. This type is used to represent a specific data
     * structure which has been declared within the code. Data structures are basic containers of data.
     *
     * Example syntax: `mut List[Int32]`
     *
     * @param name The name of the structure representing the value's memory layout.
     * @param mutability The type of mutation which is permitted on the type.
     * @param parameters The types of all the compile-time parameters the structure accepts.
     */
    data class Variable(val name: String, val mutability: Mutability, val parameters: List<AstParameterStatic>) : AstType
    
    /**
     * The union type describes a collection of types. This type encodes the meaning that a value must be exactly one of
     * a number of possible types.
     *
     * Example syntax: `Bool | Int32`
     *
     * @param types The types which a value is permitted to take, the value must be exactly one of them.
     */
    data class Union(val types: Set<AstType>) : AstType
    
    /**
     * A type as a first-class citizen. The type of the value is the type information itself, rather than an instance of
     * a specific type. This can be used to represent a function which returns a type; this is useful during compile
     * time code execution.
     */
    data object Type : AstType
}

/**
 * Templates are used to specify that an object can be polymorphic at compile-time. Objects such as structures,
 * functions, aliases, and so on can have templates applied to them. By adding templates, the object can have a single
 * implementation, but be generic across a wide range of types and values.
 */
sealed interface AstTemplate
{
    /**
     * A specific type which must be provided by the user. The type can be used to generalize which types a function or
     * a data structure can work with, i.e. the element a list can hold.
     *
     * Example syntax: `name`
     *
     * @param name The name of the parameter.
     */
    data class Type(val name: String) : AstTemplate
    
    /**
     * A specific value which must be provided by the user. The value can be used to generalize some property of a
     * function or a data structure, i.e. the length of an array.
     *
     * Example syntax: `name: Int32`
     *
     * @param name The name of the parameter.
     * @param type The type the compile-time value must be adhering to.
     * @param default The default value which should be used unless something else is specified.
     */
    data class Value(val name: String, val type: AstType, val default: AstValue?) : AstTemplate
}

/**
 * The static parameter type describes something which can be accepted to customize other objects such as data
 * structures, functions, unions, and any other customizable object. These types are used to determine which
 * compile-time parameters may be passed to other objects.
 *
 * Example syntax: `name = Int32`
 *
 * @param name The name of the parameter.
 * @param value The value associated with the compile-time parameter.
 */
data class AstParameterStatic(val name: String?, val value: AstValue)

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
data class AstParameterDynamic(val name: String, val type: AstType, val passability: Passability)
