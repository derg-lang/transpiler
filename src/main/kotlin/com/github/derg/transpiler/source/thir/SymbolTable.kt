package com.github.derg.transpiler.source.thir

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
class ThirSymbolTable(private val parent: ThirSymbolTable? = null)
{
    private val root: ThirSymbolTable = parent?.root ?: this
    private val symbols = mutableMapOf<ThirId, ThirSymbol>()
    private val identifiers = mutableMapOf<String, MutableList<ThirSymbol>>()
    
    /**
     * Registers a new [symbol], allowing the symbol to be retrieved by id or name at this scope when desired. When
     * multiple symbols are bound by the same name, the earlier bound names will be shadowed by the last bound name.
     */
    fun <Type : ThirSymbol> register(symbol: Type): Type
    {
        root.symbols[symbol.id] = symbol
        identifiers.getOrPut(symbol.name) { mutableListOf() }.add(symbol)
        return symbol
    }
    
    /**
     * Resolves the symbol with the given [id], if it exists. The symbol can be found from any symbol table in the
     * hierarchy. Note that symbol tables which do not share a common root symbol table, forms different hierarchies.
     */
    operator fun get(id: ThirId): ThirSymbol? = root.symbols[id]
    
    /**
     * Resolves the symbols with the given [name], if any exist. The order in which symbols are provided, is the order
     * in which they are seen by scope, innermost scope first. If multiple symbols with the same name is defined within
     * the same scope, the symbol declared last is placed first.
     */
    operator fun get(name: String): List<ThirSymbol>
    {
        val inner = identifiers[name] ?: emptyList()
        val outer = parent?.get(name) ?: emptyList()
        
        // Reverse inner scope, as all symbols are registered in the opposite order - outer scope is reversed too
        return inner.reversed() + outer
    }
    
    override fun toString(): String = identifiers.toString()
    override fun hashCode(): Int = identifiers.hashCode()
    override fun equals(other: Any?): Boolean = when (other)
    {
        is ThirSymbolTable -> identifiers == other.identifiers
        else               -> false
    }
}
