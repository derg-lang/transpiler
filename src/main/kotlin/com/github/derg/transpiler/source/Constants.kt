package com.github.derg.transpiler.source

/*
 * The names of the literal functions for a wide variety of builtin types. These literals are used across the entire
 * compiler, ensuring that the names of literals are easily accessible for convenience' sake.
 */
const val LIT_NAME_I32 = "i32"
const val LIT_NAME_I64 = "i64"
const val LIT_NAME_STR = "s"

/*
 * The names of the builtin data types. These names are used to refer to the specific data structures which are built
 * into the compiler. These data structures cannot be represented in source code as libraries without great effort and
 * poor support.
 */
const val TYPE_VOID = "__builtin_void"
const val TYPE_BOOL = "__builtin_bool"
const val TYPE_INT32 = "__builtin_i32"
const val TYPE_INT64 = "__builtin_i64"
const val TYPE_STR = "__builtin_str"
