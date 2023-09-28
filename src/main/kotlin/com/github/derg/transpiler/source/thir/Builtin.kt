package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.lexeme.*

/**
 * Registers all builtin types, functions, variables, everything required to implement any sort of transpiler or
 * compiler.
 */
object Builtin
{
    /**
     * Main symbol table, which holds all builtin symbols. This forms the bottom layer for all symbol resolution, and
     * all symbols in this table will be visible everywhere, in every source code file.
     *
     * Note that the symbols present in this table forms the very core language of Derg, on which everything else is
     * built on top of.
     */
    val SYMBOLS = ThirSymbolTable()
    
    /**
     * Void represents a special type that can never be instantiated. This type is used to represent the absence of a
     * value, where a value would normally be expected. This is commonly used to model functions which has no return
     * value or error, and to capture errors where an expression is used as statements.
     */
    val VOID = typeOf("__builtin_void").register(SYMBOLS) // Note: must be declared before everything else!
    
    ///////////////////
    // Builtin types //
    ///////////////////
    
    val BOOL = typeOf("__builtin_bool").register(SYMBOLS)
    val BOOL_AND = infixOperatorOf(BOOL, BOOL, SymbolType.AND).register(SYMBOLS)
    val BOOL_EQ = infixOperatorOf(BOOL, BOOL, SymbolType.EQUAL).register(SYMBOLS)
    val BOOL_NE = infixOperatorOf(BOOL, BOOL, SymbolType.NOT_EQUAL).register(SYMBOLS)
    val BOOL_NOT = prefixOperatorOf(BOOL, BOOL, SymbolType.NOT).register(SYMBOLS)
    val BOOL_OR = infixOperatorOf(BOOL, BOOL, SymbolType.OR).register(SYMBOLS)
    val BOOL_XOR = infixOperatorOf(BOOL, BOOL, SymbolType.XOR).register(SYMBOLS)
    
    val INT32 = typeOf("__builtin_int32").register(SYMBOLS)
    val INT32_LIT = literalOf("i32", INT32).register(SYMBOLS)
    val INT32_EQ = infixOperatorOf(BOOL, INT32, SymbolType.EQUAL).register(SYMBOLS)
    val INT32_GE = infixOperatorOf(BOOL, INT32, SymbolType.GREATER_EQUAL).register(SYMBOLS)
    val INT32_GT = infixOperatorOf(BOOL, INT32, SymbolType.GREATER).register(SYMBOLS)
    val INT32_LE = infixOperatorOf(BOOL, INT32, SymbolType.LESS_EQUAL).register(SYMBOLS)
    val INT32_LT = infixOperatorOf(BOOL, INT32, SymbolType.LESS).register(SYMBOLS)
    val INT32_NE = infixOperatorOf(BOOL, INT32, SymbolType.NOT_EQUAL).register(SYMBOLS)
    val INT32_ADD = infixOperatorOf(INT32, INT32, SymbolType.PLUS).register(SYMBOLS)
    val INT32_DIV = infixOperatorOf(INT32, INT32, SymbolType.DIVIDE).register(SYMBOLS)
    val INT32_MOD = infixOperatorOf(INT32, INT32, SymbolType.MODULO).register(SYMBOLS)
    val INT32_MUL = infixOperatorOf(INT32, INT32, SymbolType.MULTIPLY).register(SYMBOLS)
    val INT32_NEG = prefixOperatorOf(INT32, INT32, SymbolType.MINUS).register(SYMBOLS)
    val INT32_POS = prefixOperatorOf(INT32, INT32, SymbolType.PLUS).register(SYMBOLS)
    val INT32_SUB = infixOperatorOf(INT32, INT32, SymbolType.MINUS).register(SYMBOLS)
    
    val INT64 = typeOf("__builtin_int64").register(SYMBOLS)
    val INT64_LIT = literalOf("i64", INT64).register(SYMBOLS)
    val INT64_EQ = infixOperatorOf(BOOL, INT64, SymbolType.EQUAL).register(SYMBOLS)
    val INT64_GE = infixOperatorOf(BOOL, INT64, SymbolType.GREATER_EQUAL).register(SYMBOLS)
    val INT64_GT = infixOperatorOf(BOOL, INT64, SymbolType.GREATER).register(SYMBOLS)
    val INT64_LE = infixOperatorOf(BOOL, INT64, SymbolType.LESS_EQUAL).register(SYMBOLS)
    val INT64_LT = infixOperatorOf(BOOL, INT64, SymbolType.LESS).register(SYMBOLS)
    val INT64_NE = infixOperatorOf(BOOL, INT64, SymbolType.NOT_EQUAL).register(SYMBOLS)
    val INT64_ADD = infixOperatorOf(INT64, INT64, SymbolType.PLUS).register(SYMBOLS)
    val INT64_DIV = infixOperatorOf(INT64, INT64, SymbolType.DIVIDE).register(SYMBOLS)
    val INT64_MOD = infixOperatorOf(INT64, INT64, SymbolType.MODULO).register(SYMBOLS)
    val INT64_MUL = infixOperatorOf(INT64, INT64, SymbolType.MULTIPLY).register(SYMBOLS)
    val INT64_NEG = prefixOperatorOf(INT64, INT64, SymbolType.MINUS).register(SYMBOLS)
    val INT64_POS = prefixOperatorOf(INT64, INT64, SymbolType.PLUS).register(SYMBOLS)
    val INT64_SUB = infixOperatorOf(INT64, INT64, SymbolType.MINUS).register(SYMBOLS)
    
    // TODO: Support strings somehow
    val STR_LIT = literalOf("s", VOID).register(SYMBOLS)
}

/**
 * Simple helper function for cleaning up symbol registration.
 */
private fun <Type : ThirSymbol> Type.register(symbols: ThirSymbolTable): Type = symbols.register(this)

/**
 * Generates a new infix operator function for the given [inType], producing [outType], for the given [operator].
 */
private fun infixOperatorOf(outType: ThirType, inType: ThirType, operator: SymbolType): ThirFunction =
    functionOf(operator.symbol, valueType = outType, params = listOf(paramOf("lhs", inType), paramOf("rhs", inType)))

/**
 * Generates a new prefix operator function for the given [inType], producing [outType], for the given [operator].
 */
private fun prefixOperatorOf(outType: ThirType, inType: ThirType, operator: SymbolType): ThirFunction =
    functionOf(operator.symbol, valueType = outType, params = listOf(paramOf("rhs", inType)))

private fun functionOf(
    name: String,
    valueType: ThirType = Builtin.VOID,
    errorType: ThirType = Builtin.VOID,
    params: List<ThirParameter>,
) = ThirFunction(
    id = ThirId.Static(),
    name = name,
    valType = ThirId.Resolvable().apply { resolve(valueType.id) },
    errType = ThirId.Resolvable().apply { resolve(errorType.id) },
    params = params,
    visibility = Visibility.EXPORTED,
    scope = ThirScope(Builtin.SYMBOLS),
)

private fun literalOf(name: String, type: ThirType) = ThirLiteral(
    id = ThirId.Static(),
    name = name,
    type = type.id,
    visibility = Visibility.EXPORTED,
)

private fun paramOf(name: String, type: ThirType) = ThirParameter(
    id = ThirId.Static(),
    name = name,
    type = ThirId.Resolvable().apply { resolve(type.id) },
    defaultValue = null,
    passability = Passability.IN,
)

private fun typeOf(name: String) = ThirType(
    id = ThirId.Static(),
    name = name,
    visibility = Visibility.EXPORTED,
)
