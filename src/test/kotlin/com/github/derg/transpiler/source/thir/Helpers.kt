package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*

// Helper literals for generating an expression node from a primitive value
val Boolean.thir: ThirValueBool get() = BoolConst(this)
val Int.thir: ThirValueInt32 get() = Int32Const(this)
val Long.thir: ThirValueInt64 get() = Int64Const(this)

/**
 * Generates a function from the provided input parameters.
 */
fun thirFunOf(
    name: Name,
    valueType: ThirType = Builtin.VOID,
    errorType: ThirType = Builtin.VOID,
    params: List<ThirParameter> = emptyList(),
) = ThirFunction(
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
    type: ThirType,
    value: ThirValue? = null,
) = ThirParameter(
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
) = ThirType(
    id = IdProviderNil.random(),
    name = name,
    visibility = Visibility.PRIVATE,
)

/**
 * Generates a variable from the provided input parameters.
 */
fun thirVarOf(
    name: Name,
    type: ThirType,
) = ThirVariable(
    id = IdProviderNil.random(),
    name = name,
    type = type,
    visibility = Visibility.PRIVATE,
    mutability = Mutability.IMMUTABLE,
    assignability = Assignability.CONSTANT,
)
