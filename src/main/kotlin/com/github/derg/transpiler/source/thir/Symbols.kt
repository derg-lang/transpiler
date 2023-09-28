package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*

/**
 * Objects which may be identified in source code are known as symbols. The symbols are typically located within a scope
 * and may be resolved based on name or id.
 */
sealed interface ThirSymbol
{
    val id: ThirId.Static
    val name: String
}

/**
 * Executable code is found within functions, which form a smaller executable part of the program. Functions may take
 * any number of input parameters, and may terminate processing by returning nothing, returning a value, or by raising
 * and error.
 */
data class ThirFunction(
    override val id: ThirId.Static,
    override val name: String,
    val valType: ThirId.Resolvable,
    val errType: ThirId.Resolvable,
    val params: List<ThirParameter>,
    val visibility: Visibility,
    val scope: ThirScope,
) : ThirSymbol

/**
 * Literals are a special case of functions. In order to convert from an arbitrary constant value written into the
 * source code (i.e. any number or string), the value may be annotated with a literal which indicates how the constant
 * value should be interpreted. Literals are functions which accept exactly a single parameter, which must be of a
 * predefined set of legal types.
 */
data class ThirLiteral(
    override val id: ThirId.Static,
    override val name: String,
    val type: ThirId,
    val visibility: Visibility,
) : ThirSymbol

/**
 * The module represents a collection of symbols which are logically grouped into a cohesive unit. Every module should
 * contain symbols which together form a unit within the software.
 */
data class ThirModule(
    override val id: ThirId.Static,
    override val name: String,
    val symbols: ThirSymbolTable,
) : ThirSymbol

/**
 * The collection of a logical group of modules is known as a package, which consists of any number of source files
 * joined together in an arbitrary manner. The modules within the package are permitted circular dependencies, under the
 * condition that all symbols may be resolved in an unambiguous manner.
 */
data class ThirPackage(
    override val id: ThirId.Static,
    override val name: String,
    val symbols: ThirSymbolTable,
) : ThirSymbol

/**
 * Some functions may require additional input in order to adequately perform their functionality. This additional input
 * is modeled as parameters. A parameter is different from a variable as parameters are never stored in main memory, but
 * are rather always located on the stack.
 */
data class ThirParameter(
    override val id: ThirId.Static,
    override val name: String,
    val type: ThirId.Resolvable,
    val passability: Passability,
    val defaultValue: ThirValue?,
) : ThirSymbol

/**
 * Properties holds values within a data structure of various types. Properties may hold aa variety of different types
 * of data, such as raw values, references to other variables or functions, locations in memory, and so on.
 */
data class ThirProperty(
    override val id: ThirId.Static,
    override val name: String,
    val type: ThirId,
    val visibility: Visibility,
    val mutability: Mutability,
    val assignability: Assignability,
) : ThirSymbol

/**
 * All data within a program must be represented as a type. Every type requires some amount of physical space in memory,
 * which is used to allocate instances of the type on the heap or the stack. Types may be instantiated as variables or
 * parameters, which may be accessed or modified as needed.
 */
data class ThirType(
    override val id: ThirId.Static,
    override val name: String,
    val visibility: Visibility,
) : ThirSymbol

/**
 * Variables holds values stored in memory at some arbitrary location. Variables may hold aa variety of different types
 * of data, such as raw values, references to other variables or functions, locations in memory, and so on. Variables
 * may be located either on the stack, or in the heap.
 */
data class ThirVariable(
    override val id: ThirId.Static,
    override val name: String,
    val type: ThirId.Resolvable,
    val visibility: Visibility,
    val mutability: Mutability,
    val assignability: Assignability,
) : ThirSymbol

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Not-quite-symbols-but-close-enough
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Represents an argument passed into a function call. The argument may be provided with a [name], although it will
 * always be given a [value].
 */
data class ThirArgument(val name: String?, val value: ThirValue)

/**
 * Scopes defines a region of codebase independent of other scopes at the same depth. Scopes may inherit all named
 * objects from parent scopes; the scope may read all symbols defined in the [symbols] table. The scope contains a set
 * of executable [instructions].
 *
 * @param parent The symbol table which originates from the outer scope.
 */
class ThirScope(parent: ThirSymbolTable)
{
    val symbols = ThirSymbolTable(parent)
    val instructions = mutableListOf<ThirInstruction>()
    
    override fun toString(): String = "{instructions=$instructions, symbols=$symbols}"
    override fun hashCode(): Int = instructions.hashCode()
    override fun equals(other: Any?): Boolean = when (other)
    {
        is ThirScope -> instructions == other.instructions && symbols == other.symbols
        else         -> false
    }
}
