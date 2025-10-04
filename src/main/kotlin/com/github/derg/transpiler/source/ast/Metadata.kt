package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.*

/**
 * TODO: Write me.
 */
sealed interface AstKind
{
    /**
     * TODO: Write me.
     */
    data object Nothing : AstKind
    
    /**
     * TODO: Write me.
     */
    data object Type : AstKind
    
    /**
     * TODO: Write me.
     */
    data class Value(val type: AstType) : AstKind
}

/**
 * All expressions are granted exactly one type of all possibilities. Certain symbols within the codebase, such as
 * variables, parameters, fields, functions, and so on have their own types. The type information can be used to verify
 * that the program is well-typed, avoiding errors such as passing the wrong type as a function parameter.
 */
sealed interface AstType
{
    /**
     * The expression type represents a structure with a specific layout of data in memory. This type is used to
     * represent a data structure which has been declared within the code. Data structures are basic containers of data.
     *
     * Example syntax: `List[Int32] | Bool | Int32`
     *
     * @param value Any legal expression which may evaluate to a legal type.
     */
    data class Expression(val value: AstValue) : AstType
    
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
    data class Function(val value: AstType?, val error: AstType?, val parameters: List<Parameter>) : AstType
    
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
    data class Parameter(val name: String, val type: AstType, val passability: Passability)
}
