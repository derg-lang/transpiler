package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.Constant
import com.github.derg.transpiler.source.ast.Expression

val Boolean.e: Expression get() = Constant.Bool(this)
val Boolean.v: ValueBool get() = BoolConst(this)
val Int.e: Expression get() = Constant.Real(this, Builtin.LIT_INT32)
val Int.v: ValueInt32 get() = Int32Const(this)
val Long.e: Expression get() = Constant.Real(this, Builtin.LIT_INT64)
val Long.v: ValueInt64 get() = Int64Const(this)

/**
 * Generates a function from the provided input parameters.
 */
fun functionOf(
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
fun parameterOf(
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
 * Generates a variable from the provided input parameters.
 */
fun variableOf(
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
