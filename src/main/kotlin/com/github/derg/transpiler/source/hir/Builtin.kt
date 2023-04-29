package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.Id
import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.Visibility

/**
 * Registers all builtin types, functions, variables, everything required to implement any sort of transpiler or
 * compiler.
 */
object Builtin
{
    val SYMBOLS = SymbolTable()
    
    val BOOL = SYMBOLS.register(typeOf("__builtin_bool", size = 1))
    val INT32 = SYMBOLS.register(typeOf("__builtin_int32", size = 4))
    val INT64 = SYMBOLS.register(typeOf("__builtin_int64", size = 8))
    val VOID = SYMBOLS.register(typeOf("__builtin_void", size = 0))
    
    const val LIT_INT32 = "i32"
    const val LIT_INT64 = "i64"
}

/**
 * Generates a new type, with the given [size] in bytes and a specific [name].
 */
private fun typeOf(name: Name, size: Int) = Type(
    id = Id.randomUUID(),
    name = name,
    visibility = Visibility.EXPORTED,
    size = size,
)
