package com.github.derg.transpiler.phases.resolver

import java.util.*

/**
 * Scopes represents the symbols which are accessible at a certain layer in the source code. Scopes may be nested inside
 * each other, forming a hierarchy of scopes.
 */
class Scope
{
    private val nameToSymbols = mutableMapOf<String, MutableSet<UUID>>()
    
    /**
     * Registers a [name] in this scope, bound to the given [id]. If multiple symbols are registered under the same
     * name, they will all be overloaded in this scope. That is, the same name may refer to multiple symbols at the
     * same time.
     */
    fun register(id: UUID, name: String)
    {
        nameToSymbols.getOrPut(name) { mutableSetOf() }.add(id)
    }
    
    /**
     * Retrieves all symbols which are registered under the given name.
     */
    fun find(name: String): Set<UUID>
    {
        return nameToSymbols[name].orEmpty()
    }
}
