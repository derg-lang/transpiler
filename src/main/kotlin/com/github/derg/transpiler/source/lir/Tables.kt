package com.github.derg.transpiler.source.lir

import com.github.derg.transpiler.source.Id
import com.github.derg.transpiler.source.Name

/**
 * Unique identifier for each and every identifiable object. No two objects will ever be granted the same id.
 */
private var RUNNING_ID_NUMBER = 0

/**
 * All symbols are uniquely identified by their id, and are instantiated at various times during the analysis phase of
 * the source code. Typically, the root symbols are registered in the module translation table before being registered
 * to the symbol table.
 */
class SymbolTable
{
    private val symbols = mutableMapOf<Id, Symbol>()
    
    /**
     * Registers the given [symbol] under its own id. Note that symbols can only be instantiated once they have been
     * assigned an id. Multiple symbols cannot be registered under the same id.
     */
    fun register(symbol: Symbol)
    {
        require(symbol.id !in symbols) { "Cannot register two symbols with the same id" }
        
        symbols[symbol.id] = symbol
    }
    
    /**
     * Retrieves the symbol with the given [id], if it exists.
     */
    fun resolve(id: Id): Symbol? = symbols[id]
}

/**
 * All scopes may contain their own declarations of variables, functions, types, and so on. Each scope must track which
 * object id is associated with each name, allowing each scope to hold arbitrary symbols.
 *
 * Scopes may be nested in other parent scopes, which impacts resolution of an identifier. If an identifier is not
 * found in the current scope, all parent scopes are checked recursively to the root scope. In effect, a new scope
 * allows a previously defined identifier to be shadowed.
 *
 * @param parent The parent scope translation table, containing all inherited names from the outer scope.
 */
class TranslationTable(private val parent: TranslationTable? = null)
{
    private val ids = mutableMapOf<Name, MutableList<Id>>()
    
    /**
     * Registers the [name] of a new symbol within this scope, returning the id of the symbol. The name of the symbol
     * must be registered in order to produce the id of the symbol, which will be used to generate the symbol itself.
     */
    fun <T : Id> register(name: Name, factory: (Int) -> T): T
    {
        val id = factory(RUNNING_ID_NUMBER++)
        ids.getOrPut(name) { mutableListOf() }.add(id)
        return id
    }
    
    /**
     * Resolves the ids of all symbols with the given [name]. Note that the order of ids are provided in the same order
     * in which the symbols were registered *by scope*, then *by name*. The outermost scope is resolved first, followed
     * by all ids resolved from the innermost scope.
     *
     * In effect, the last registered name is found at the last index in the resolved list of ids.
     */
    fun resolve(name: Name): List<Id>
    {
        val outermost = parent?.resolve(name) ?: emptyList()
        val innermost = ids[name] ?: emptyList()
        return outermost + innermost
    }
}
