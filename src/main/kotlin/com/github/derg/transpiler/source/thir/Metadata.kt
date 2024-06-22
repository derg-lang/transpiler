package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import java.util.*

/**
 * Represents an instance of some symbol, either directly by name or indirectly via some computed value. The instance
 * may refer to any symbol within the codebase a developer is working in. Instances are used to determine which object
 * should be accessed, i.e. which variable should be assigned when performing an assignment instruction, which function
 * to invoke when calling a computed value, etc.
 */
sealed interface ThirInstance
{
    /**
     * The instance is something which is referenced directly by [symbolId]. Typically, this is the case when a
     * developer is invoking a function directly, or accessing the value of a variable, or similar. The instance may be
     * referenced with any number of [generics].
     *
     * @param symbolId The actual symbol this instance is. Typically, it is a function, struct, or variable of sorts.
     * @param generics The compile-time parameters which have been provided during instance resolution.
     */
    data class Named(val symbolId: UUID, val generics: List<Generic>) : ThirInstance
    
    /**
     * The instance is something which is computed from a given [value]. This case is encountered when a developer
     * attempts to invoke a callable from something evaluating to a function, writing to a memory address obtained from
     * a function call, or similar.
     *
     * @param value The value which describes how to compute the actual instance.
     */
    data class Value(val value: ThirValue) : ThirInstance
    
    /**
     * When instances are resolved fully, the generic parameters may resolve either to a type or a value. Here, we
     * capture both possible cases as a union.
     */
    sealed interface Generic
    {
        data class Type(val type: ThirType) : Generic
        data class Value(val value: ThirValue) : Generic
    }
}

/**
 * Generics represents information which is known at compile-time, and can be substituted for other bits of
 * information on the use-site. Developers may use generics to write code in a general manner, such that it can be
 * used across a vast number of types and values.
 */
sealed interface ThirTemplate
{
    /**
     * The generic represents a type which is determined at compile-time. All usages of the generic refers to the given
     * type at the use site, allowing a developer to parameterize code across a selection of types.
     */
    data object Type : ThirTemplate
    
    /**
     * The generic represents a value of the [type] determined at compile-time. All usages of the generic refers to the
     * given value at the use site, allowing a developer to parameterize code across a selection of values.
     */
    data class Value(val type: ThirType) : ThirTemplate
}

/**
 * Type annotations indicates which type a specific object is. All variables must be declared with a specific type,
 * either by the developer or by the compiler. In many cases, the compiler can deduce the type, although the developer
 * must have the option of specifying the type when it is ambiguous.
 */
sealed interface ThirType
{
    /**
     * The function type describes a function returning a [valueType] and [errorType], with any number of [parameters].
     * The parameters are required to have a valid value, and are not permitted to have any error type associated with
     * them.
     */
    data class Call(val valueType: ThirType?, val errorType: ThirType?, val parameters: List<Parameter>) : ThirType
    
    /**
     * The struct type describes a concrete type by [symbolId], which may be specialized with any number of [generics].
     * The data within the type has a certain [mutability], indicating whether the type is internally mutable or not.
     */
    data class Data(val symbolId: UUID, val mutability: Mutability, val generics: List<Generic>) : ThirType
    
    /**
     * Generics represents information which is known at compile-time, and can be substituted for other bits of
     * information on the use-site. Developers may use generics to write code in a general manner, such that it can be
     * used across a vast number of types and values.
     */
    data class Generic(val name: String, val template: ThirTemplate)
    
    /**
     * The parameter type describes the [name] and [type] of the input parameter. The parameter is used during call
     * resolution, to disambiguate which callable to invoke.
     */
    data class Parameter(val name: String, val type: ThirType)
}
