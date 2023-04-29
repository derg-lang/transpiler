package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.Constant
import com.github.derg.transpiler.source.ast.Expression
import com.github.derg.transpiler.source.hir.SymbolTable

val Boolean.e: Expression get() = Constant.Bool(this)
val Boolean.v: ValueBool get() = BoolConst(this)
val Int.e: Expression get() = Constant.Real(this, Builtin.LIT_INT32)
val Int.v: ValueInt32 get() = Int32Const(this)
val Long.e: Expression get() = Constant.Real(this, Builtin.LIT_INT64)
val Long.v: ValueInt64 get() = Int64Const(this)

fun variableOf(
    name: Name,
    type: Id,
) = Variable(
    id = Id.randomUUID(),
    name = name,
    visibility = Visibility.PRIVATE,
    mutability = Mutability.IMMUTABLE,
    assignability = Assignability.CONSTANT,
    type = type,
)

fun functionOf(
    name: Name,
    valueType: Id = Builtin.VOID.id,
    errorType: Id = Builtin.VOID.id,
) = Function(
    id = Id.randomUUID(),
    name = name,
    visibility = Visibility.PRIVATE,
    value = valueType,
    error = errorType,
    params = emptyList(),
    symbols = SymbolTable(Builtin.SYMBOLS),
)
