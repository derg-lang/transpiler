package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*

/**
 * The different kinds for types in the type system.
 */
sealed interface HirKind
{
    /**
     * The kind of nothing. Any symbol which evaluates to this kind cannot ever be permitted to provide any value of any
     * sort.
     */
    data object Nothing : HirKind
    
    /**
     * The kind of types which are permitted to have values. This is for example "complete" types, such as `double`,
     * `bool`, `int`, `list<int>`, and so on.
     */
    data object Type : HirKind
    
    /**
     * The kind of values as type parameters. All type parameters of this kind are shaped according to the [type] they
     * are associated with.
     */
    data class Value(val type: HirType) : HirKind
}

/**
 * The base type representation for the type system.
 */
sealed interface HirType
{
    /**
     * Expressions represents a type which has not yet been computed, or some constant term which should be used as a
     * compile-time value.
     *
     * @param value The value which the type should be considered to be.
     */
    data class Expression(val value: HirExpression) : HirType
    
    /**
     * Function types, which describes a callable object. Functions are permitted to return either a value type, or an
     * error type, never both. Functions are allowed to take an arbitrary number of parameters.
     *
     * @param valueType The value type returned by this callable.
     * @param errorType The error type returned by this callable.
     * @param parameters The parameters which are acceptable by this callable.
     */
    data class Function(val valueType: HirType?, val errorType: HirType?, val parameters: List<Parameter>) : HirType
    
    /**
     * Parameters represents an input into a callable object.
     *
     * @param name The name of the parameter.
     * @param type The type information of the value provided with this parameter.
     * @param default The default value of the parameter, if any.
     * @param passability The specific mechanism of how the parameter should be passed into the function.
     */
    data class Parameter(val name: String, val type: HirType, val default: HirExpression?, val passability: Passability)
}
