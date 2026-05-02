package com.github.derg.transpiler.experimental.resolver

import java.util.*

/**
 * The kind of type an object is.
 */
sealed interface ThirKind
{
    /**
     * The kind of nothing. Any symbol which evaluates to this kind cannot ever be permitted to provide any value of any
     * sort.
     */
    data object Nothing : ThirKind
    
    /**
     * The kind of types which are permitted to have values. This is for example "complete" types, such as `double`,
     * `bool`, `int`, `list<int>`, and so on.
     */
    data object Type : ThirKind
    
    /**
     * The kind of values as type parameters. All type parameters of this kind are shaped according to the [type] they
     * are associated with.
     */
    data class Value(val type: ThirType) : ThirKind
    
    /**
     * A placeholder kind variable. Used during type-checking to represent an unresolved kind.
     */
    data class Var(val nodeId: UUID) : ThirKind
}

/**
 * The types which can be expressed in the type system.
 */
sealed interface ThirType
{
    /**
     * The boolean values, can hold either `true` or `false`.
     */
    data object Bool : ThirType
    
    /**
     * Single-precision floating point numbers.
     */
    data object Float32 : ThirType
    
    /**
     * Double-precision floating point numbers.
     */
    data object Float64 : ThirType
    
    /**
     * 32-bit wide signed integer.
     */
    data object Int32 : ThirType
    
    /**
     * 64-bit wide signed integer.
     */
    data object Int64 : ThirType
    
    /**
     * Unicode string.
     */
    data object Str : ThirType
    
    /**
     * The type of function.
     */
    data class Function(
        val parameters: List<ThirKind>,
        val valueKind: ThirKind,
        val errorKind: ThirKind,
    ) : ThirType
    
    /**
     * The type of structure.
     */
    data class Instance(
        val typeParameters: List<ThirExpression.Canonical>,
        val fields: Map<String, ThirKind>,
    ) : ThirType
}

/*
fun foo(): Int32 -> Bool { ... }
fun foo(in value: Bool): Str { ... }

val test: foo() = ... // Does not compile, `foo()` is not `type`
val test: Int32 = ... // Does compile, `Int32` is `type`
*/
