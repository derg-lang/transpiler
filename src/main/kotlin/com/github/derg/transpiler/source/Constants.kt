package com.github.derg.transpiler.source

/*
 * The names of the literal functions for a wide variety of builtin types. These literals are used across the entire
 * compiler, ensuring that the names of literals are easily accessible for convenience' sake.
 */
const val INT32_LIT_NAME = "i32"
const val INT64_LIT_NAME = "i64"
const val STR_LIT_NAME = "s"

/*
 * The names of the builtin data types. These names are used to refer to the specific data structures which are built
 * into the compiler. These data structures cannot be represented in source code as libraries without great effort and
 * poor support.
 */
const val VOID_TYPE_NAME = "__builtin_void"
const val BOOL_TYPE_NAME = "__builtin_bool"
const val INT32_TYPE_NAME = "__builtin_i32"
const val INT64_TYPE_NAME = "__builtin_i64"
const val STR_TYPE_NAME = "__builtin_str"

const val DIVIDE_BY_ZERO_TYPE_NAME = "__builtin_divide_by_zero"
