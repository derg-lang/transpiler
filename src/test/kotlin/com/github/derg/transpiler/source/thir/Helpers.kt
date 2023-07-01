package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*

// Helper literals for generating an expression node from a primitive value
val Boolean.thir: ValueBool get() = BoolConst(this)
val Int.thir: ValueInt32 get() = Int32Const(this)
val Long.thir: ValueInt64 get() = Int64Const(this)

/**
 * Generates a function from the provided input parameters.
 */
fun thirFunOf(
    name: Name,
    valueType: Type = Builtin.VOID,
    errorType: Type = Builtin.VOID,
    params: List<Function.Parameter> = emptyList(),
) = Function(
    id = IdProviderNil.random(),
    name = name,
    value = valueType,
    error = errorType,
    params = params,
    visibility = Visibility.PRIVATE,
)

/**
 * Generates a parameter from the provided input parameters.
 */
fun thirParOf(
    name: Name,
    type: Type,
    value: Value? = null,
) = Function.Parameter(
    id = IdProviderNil.random(),
    name = name,
    type = type,
    value = value,
    passability = Passability.IN,
)

/**
 * Generates a type from the provided input parameters.
 */
fun thirTypeOf(
    name: Name,
) = Type(
    id = IdProviderNil.random(),
    name = name,
    visibility = Visibility.PRIVATE,
)

/**
 * Generates a variable from the provided input parameters.
 */
fun thirVarOf(
    name: Name,
    type: Type,
) = Variable(
    id = IdProviderNil.random(),
    name = name,
    type = type,
    visibility = Visibility.PRIVATE,
    mutability = Mutability.IMMUTABLE,
    assignability = Assignability.CONSTANT,
)
