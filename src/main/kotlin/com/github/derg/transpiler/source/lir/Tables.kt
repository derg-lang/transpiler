package com.github.derg.transpiler.source.lir

import com.github.derg.transpiler.source.Id
import com.github.derg.transpiler.source.Name

/**
 * Unique identifier for each and every identifiable object. No two objects will ever be granted the same id.
 */
private var RUNNING_ID_NUMBER = 0

/**
 * All scopes may contain their own declarations of variables, functions, types, and so on. Each scope must track which
 * object id is associated with each name, allowing each scope to hold.
 *
 * Scopes may be nested in other parent scopes, which impacts resolution of an identifier. If an identifier is not
 * found in the current scope, all parent scopes are checked recursively to the root scope. In effect, a new scope
 * allows a previously defined identifier to be shadowed.
 */
class SymbolTable
{
    private val idToName = mutableMapOf<Id, Name>()
    private val nameToId = mutableMapOf<Name, MutableSet<Id>>()
    
    /**
     * Registers the [name] of a new symbol within this scope, returning the id of the symbol.
     */
    fun register(name: Name, factory: (Int) -> Id): Id
    {
        val id = factory(RUNNING_ID_NUMBER++)
        idToName[id] = name
        nameToId.getOrPut(name) { mutableSetOf() }.add(id)
        return id
    }
    
    /**
     * Resolves the id of the symbol with the given [name].
     */
    fun resolve(name: Name): Set<Id>
    {
        return nameToId[name] ?: emptySet()
    }
    
    override fun toString(): String = idToName.toString()
    override fun hashCode(): Int = idToName.hashCode()
    override fun equals(other: Any?): Boolean = when (other)
    {
        is SymbolTable -> idToName == other.idToName
        else           -> false
    }
}
