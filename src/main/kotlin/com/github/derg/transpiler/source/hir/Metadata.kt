package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.utils.*

/**
 * Represents an instance of some symbol, either directly by name or indirectly via some computed value. The instance
 * may refer to any symbol within the codebase a developer is working in. Instances are used to determine which object
 * should be accessed, i.e. which variable should be assigned when performing an assignment instruction, which function
 * to invoke when calling a computed value, etc.
 */
sealed interface HirInstance
{
    /**
     * The instance is something which is referenced directly by [name]. Typically, this is the case when a developer is
     * invoking a function directly, or accessing the value of a variable, or similar. The instance may be referenced
     * with any number of [generics].
     */
    data class Named(val name: String, val generics: List<NamedMaybe<HirValue>>) : HirInstance
    
    /**
     * The instance is something which is computed from a given [value]. This case is encountered when a developer
     * attempts to invoke a callable from something evaluating to a function, writing to a memory address obtained from
     * a function call, or similar.
     */
    data class Value(val value: HirValue) : HirInstance
}

/**
 * Generics represents information which is known at compile-time, and can be substituted for other bits of
 * information on the use-site. Developers may use generics to write code in a general manner, such that it can be
 * used across a vast number of types and values.
 */
sealed interface HirTemplate
{
    /**
     * The generic represents a type which is determined at compile-time. All usages of the generic refers to the given
     * type at the use site, allowing a developer to parameterize code across a selection of types.
     */
    data object Type : HirTemplate
    
    /**
     * The generic represents a [type] which is determined at compile-time. All usages of the generic refers to the
     * given value at the use site, allowing a developer to parameterize code across a selection of values.
     */
    data class Value(val type: HirType) : HirTemplate
}

/**
 * Type annotations indicates which type a specific object is. All variables must be declared with a specific type,
 * either by the developer or by the compiler. In many cases, the compiler can deduce the type, although the developer
 * must have the option of specifying the type when it is ambiguous.
 *
 * Types are only every of two variants; either it represents some data arranged in a specific way in memory, or it
 * references a signature for callables. Note that memory layout may be a struct, union, alias, or similar. Callables
 * may be functions, literals, or similar.
 */
sealed interface HirType
{
    /**
     * The function type describes a function returning a [valueType] and [errorType], with any number of [parameters].
     */
    data class Call(val valueType: HirType?, val errorType: HirType?, val parameters: List<Parameter>) : HirType
    
    /**
     * The struct type describes a concrete type by [name], which may be specialized with any number of [generics]. The
     * data within the type has a certain [mutability], indicating whether the type is internally mutable or not.
     */
    data class Data(val name: String, val mutability: Mutability, val generics: List<Generic>) : HirType
    
    /**
     * Generics represents information which is known at compile-time, and can be substituted for other bits of
     * information on the use-site. Developers may use generics to write code in a general manner, such that it can be
     * used across a vast number of types and values.
     */
    data class Generic(val name: String, val template: HirTemplate)
    
    /**
     * The parameter type describes the [name] and [type] of the input parameter.
     */
    data class Parameter(val name: String, val type: HirType)
}
