package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import java.util.*

/**
 * The collection of all builtin types, functions, literals, and everything else. The builtin scope contains everything
 * which is needed to compile any source code into a valid program.
 */
object Builtin
{
    /**
     * Main entrance to any sort of symbol resolution begins at the global scope level. All other symbols must be
     * registered in a scope nested from the global scope in order to be valid. The global scope will contain all
     * builtin symbols, and should never have any non-builtin symbols registered into it.
     */
    val GLOBAL_SCOPE = Scope(null)
    
    /**
     * Void represents a special type that can never be instantiated. This type is used to represent the absence of a
     * value, where a value would normally be expected. This is commonly used to model functions which has no return
     * value or error, and to capture errors where an expression is used as statements.
     */
    val VOID = registerType(VOID_TYPE_NAME)
    
    val BOOL = registerType(BOOL_TYPE_NAME)
    val BOOL_AND = registerInfixOp(Symbol.AND, BOOL, BOOL, null)
    val BOOL_EQ = registerInfixOp(Symbol.EQUAL, BOOL, BOOL, null)
    val BOOL_NE = registerInfixOp(Symbol.NOT_EQUAL, BOOL, BOOL, null)
    val BOOL_NOT = registerPrefixOp(Symbol.NOT, BOOL, BOOL, null)
    val BOOL_OR = registerInfixOp(Symbol.OR, BOOL, BOOL, null)
    val BOOL_XOR = registerInfixOp(Symbol.XOR, BOOL, BOOL, null)
    
    val INT32 = registerType(INT32_TYPE_NAME)
    val INT32_LIT = registerLiteral(INT32_LIT_NAME, INT32)
    val INT32_EQ = registerInfixOp(Symbol.EQUAL, INT32, BOOL, null)
    val INT32_GE = registerInfixOp(Symbol.GREATER_EQUAL, INT32, BOOL, null)
    val INT32_GT = registerInfixOp(Symbol.GREATER, INT32, BOOL, null)
    val INT32_LE = registerInfixOp(Symbol.LESS_EQUAL, INT32, BOOL, null)
    val INT32_LT = registerInfixOp(Symbol.LESS, INT32, BOOL, null)
    val INT32_NE = registerInfixOp(Symbol.NOT_EQUAL, INT32, BOOL, null)
    val INT32_ADD = registerInfixOp(Symbol.PLUS, INT32, INT32, VOID)
    val INT32_DIV = registerInfixOp(Symbol.DIVIDE, INT32, INT32, VOID)
    val INT32_MOD = registerInfixOp(Symbol.MODULO, INT32, INT32, VOID)
    val INT32_MUL = registerInfixOp(Symbol.MULTIPLY, INT32, INT32, VOID)
    val INT32_NEG = registerPrefixOp(Symbol.MINUS, INT32, INT32, VOID)
    val INT32_POS = registerPrefixOp(Symbol.PLUS, INT32, INT32, VOID)
    val INT32_SUB = registerInfixOp(Symbol.MINUS, INT32, INT32, VOID)
    
    val INT64 = registerType(INT64_TYPE_NAME)
    val INT64_LIT = registerLiteral(INT64_LIT_NAME, INT64)
    val INT64_EQ = registerInfixOp(Symbol.EQUAL, INT64, BOOL, null)
    val INT64_GE = registerInfixOp(Symbol.GREATER_EQUAL, INT64, BOOL, null)
    val INT64_GT = registerInfixOp(Symbol.GREATER, INT64, BOOL, null)
    val INT64_LE = registerInfixOp(Symbol.LESS_EQUAL, INT64, BOOL, null)
    val INT64_LT = registerInfixOp(Symbol.LESS, INT64, BOOL, null)
    val INT64_NE = registerInfixOp(Symbol.NOT_EQUAL, INT64, BOOL, null)
    val INT64_ADD = registerInfixOp(Symbol.PLUS, INT64, INT64, VOID)
    val INT64_DIV = registerInfixOp(Symbol.DIVIDE, INT64, INT64, VOID)
    val INT64_MOD = registerInfixOp(Symbol.MODULO, INT64, INT64, VOID)
    val INT64_MUL = registerInfixOp(Symbol.MULTIPLY, INT64, INT64, VOID)
    val INT64_NEG = registerPrefixOp(Symbol.MINUS, INT64, INT64, VOID)
    val INT64_POS = registerPrefixOp(Symbol.PLUS, INT64, INT64, VOID)
    val INT64_SUB = registerInfixOp(Symbol.MINUS, INT64, INT64, VOID)
    
    // TODO: Support strings somehow
    val STR = registerType(STR_TYPE_NAME)
    val STR_LIT = registerLiteral(STR_LIT_NAME, VOID)
}

/**
 * Registers a new type with the given [name].
 */
private fun registerType(name: String) = HirStruct(
    id = UUID.randomUUID(),
    name = name,
    visibility = Visibility.EXPORTED,
    fields = emptyList(),
    methods = emptyList(),
    generics = emptyList(),
).also(Builtin.GLOBAL_SCOPE::register)

/**
 * Defines a new literal with the given [name] and [type].
 */
private fun registerLiteral(name: String, type: HirStruct) = HirLiteral(
    id = UUID.randomUUID(),
    name = name,
    visibility = Visibility.EXPORTED,
    value = HirTypeStruct(type.name, emptyList(), Mutability.IMMUTABLE),
    instructions = emptyList(),
    variables = emptyList(),
    parameter = paramOf("value", type),
).also(Builtin.GLOBAL_SCOPE::register)

/**
 * Defines a new infix operator for the given [operator]. The [parameter] type will be the same for both the left- and
 * the right-hand side expressions. The operator returns a value of the given [value] and [error] types.
 */
private fun registerInfixOp(operator: Symbol, parameter: HirStruct, value: HirStruct, error: HirStruct?) = HirFunction(
    id = UUID.randomUUID(),
    name = operator.symbol,
    visibility = Visibility.EXPORTED,
    value = HirTypeStruct(value.name, emptyList(), Mutability.IMMUTABLE),
    error = error?.let { HirTypeStruct(it.name, emptyList(), Mutability.IMMUTABLE) },
    instructions = emptyList(),
    generics = emptyList(),
    variables = emptyList(),
    parameters = listOf(paramOf("lhs", parameter), paramOf("rhs", parameter)),
).also(Builtin.GLOBAL_SCOPE::register)

/**
 * Defines a new prefix operator for the given [operator]. The [parameter] type determines which expressions are legal.
 * The operator returns a value of the given [value] and [error] types.
 */
private fun registerPrefixOp(operator: Symbol, parameter: HirStruct, value: HirStruct, error: HirStruct?) = HirFunction(
    id = UUID.randomUUID(),
    name = operator.symbol,
    visibility = Visibility.EXPORTED,
    value = HirTypeStruct(value.name, emptyList(), Mutability.IMMUTABLE),
    error = error?.let { HirTypeStruct(it.name, emptyList(), Mutability.IMMUTABLE) },
    instructions = emptyList(),
    generics = emptyList(),
    variables = emptyList(),
    parameters = listOf(paramOf("rhs", parameter)),
).also(Builtin.GLOBAL_SCOPE::register)

/**
 * Defines a new function parameter of the given [type].
 */
private fun paramOf(name: String, type: HirStruct) = HirParameter(
    id = UUID.randomUUID(),
    name = name,
    passability = Passability.IN,
    type = HirTypeStruct(type.name, emptyList(), Mutability.IMMUTABLE),
    value = null,
)
