package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*

/**
 * Objects which may be identified in source code are known as symbols. The symbols are typically located within a scope
 * and may be resolved based on name or id.
 */
sealed interface Symbol
{
    val id: Id
    val name: Name
}

/**
 * The collection of a logical group of modules is known as a package, which consists of any number of source files
 * joined together in an arbitrary manner. The modules within the package are permitted circular dependencies, under the
 * condition that all symbols may be resolved in an unambiguous manner.
 */
data class Package(
    override val id: Id,
    override val name: Name,
    val symbols: SymbolTable,
) : Symbol

/**
 * The module represents a collection of symbols which are logically grouped into a cohesive unit. Every module should
 * contain symbols which together form a unit within the software.
 */
data class Module(
    override val id: Id,
    override val name: Name,
) : Symbol
{
    /**
     * The symbols and instructions associated with the module will be assigned when the module is defined. The scope
     * will hold all such information.
     */
    lateinit var scope: Scope
}

/**
 * All data within a program must be represented as a type. Every type requires some amount of physical space in memory,
 * which is used to allocate instances of the type on the heap or the stack. Types may be instantiated as variables or
 * parameters, which may be accessed or modified as needed.
 */
data class Type(
    override val id: Id,
    override val name: Name,
    val visibility: Visibility,
) : Symbol
{
    /**
     * The size of the type in number of bytes, all nested properties included.
     */
    var size: Int = 0
}

/**
 * Executable code is found within functions, which form a smaller executable part of the program. Functions may take
 * any number of input parameters, and may terminate processing by returning nothing, returning a value, or by raising
 * and error.
 */
data class Function(
    override val id: Id,
    override val name: Name,
    val value: Type,
    val error: Type,
    val params: List<Parameter>,
    val visibility: Visibility,
) : Symbol
{
    // TODO: Move this out of the function and into the root level of this file
    data class Parameter(
        override val id: Id,
        override val name: Name,
        val type: Type,
        val value: Value?,
        val passability: Passability,
    ) : Symbol
    
    /**
     * The symbols and instructions associated with the function will be assigned when the function is defined. The
     * scope will hold all such information.
     */
    lateinit var scope: Scope
}

/**
 * Variables holds values stored in memory at some arbitrary location. Variables may hold aa variety of different types
 * of data, such as raw values, references to other variables or functions, locations in memory, and so on.
 */
data class Variable(
    override val id: Id,
    override val name: Name,
    val type: Type,
    val visibility: Visibility,
    val mutability: Mutability,
    val assignability: Assignability,
) : Symbol

/**
 * All symbols within a single source program must be uniquely identifiable in some manner. A symbol does not
 * necessarily refer to a unique instance of a function, but instead represents a source code object. This may be a
 * type, function, variable, template, or any other identifiable object.
 *
 * All scopes may contain their own declarations of variables, functions, types, and so on. Each scope must track which
 * object id is associated with each name, allowing each scope to hold arbitrary symbols.
 *
 * Scopes may be nested in other parent scopes, which impacts resolution of an identifier. If an identifier is not
 * found in the current scope, all parent scopes are checked recursively to the root scope. In effect, a new scope
 * allows a previously defined identifier to be shadowed.
 *
 * @property parent The parent scope symbol table, containing all inherited names from the outer scope.
 */
class SymbolTable(private val parent: SymbolTable? = null)
{
    private val identifiers = mutableMapOf<Name, MutableList<Symbol>>()
    
    /**
     * Registers a new [symbol], allowing the symbol to be retrieved by id or name at this scope when desired. When
     * multiple symbols are bound by the same name, the earlier bound names will be shadowed by the last bound name.
     */
    fun <Type : Symbol> register(symbol: Type): Type
    {
        identifiers.getOrPut(symbol.name) { mutableListOf() }.add(symbol)
        return symbol
    }
    
    /**
     * Resolves the symbols with the given [name], if any exist. The order in which symbols are provided, is the order
     * in which they are seen by scope, innermost scope first. If multiple symbols with the same name is defined within
     * the same scope, the symbol declared last is placed first.
     */
    // TODO: Rename method to `resolve` or something similar
    fun find(name: Name): List<Symbol>
    {
        val inner = identifiers[name] ?: emptyList()
        val outer = parent?.find(name) ?: emptyList()
        
        // Reverse inner scope, as all symbols are registered in the opposite order - outer scope is reversed too
        return inner.reversed() + outer
    }
}

/**
 * Scopes defines a region of codebase independent of other scopes at the same depth. Scopes may inherit all named
 * objects from parent scopes; the scope may read all symbols defined in the [symbols] table. The scope contains a set
 * of executable [instructions].
 */
class Scope(val instructions: List<Instruction>, val symbols: SymbolTable)
{
    override fun hashCode(): Int = instructions.hashCode()
    override fun equals(other: Any?): Boolean = when (other)
    {
        is Scope -> this.instructions == other.instructions
        else     -> false
    }
}
