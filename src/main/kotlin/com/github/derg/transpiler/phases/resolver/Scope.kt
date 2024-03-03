package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.hir.*
import java.util.*

/**
 * Scopes represents the symbols which are accessible at a certain layer in the source code. Scopes may be nested inside
 * each other, forming a hierarchy of scopes.
 *
 * @param parent The outer scope in which this scope lives in.
 */
class Scope(private val parent: Scope?)
{
    private val _symbols = mutableMapOf<String, MutableList<HirSymbol>>()
    
    /**
     * The collection of symbols which have been registered into this scope.
     */
    val symbols: List<HirSymbol>
        get() = _symbols.values.flatten()
    
    /**
     * Registers the [symbol] id in this scope. Note that each symbol must only be registered once.
     */
    fun register(symbol: HirSymbol)
    {
        _symbols.getOrPut(symbol.name) { mutableListOf() }.add(symbol)
    }
    
    /**
     * Resolves the given [name] to all symbols visible from the current scope, including all outer scopes.
     */
    fun resolve(name: String): List<HirSymbol>
    {
        val inner = _symbols[name] ?: emptyList()
        val outer = parent?.resolve(name) ?: emptyList()
        
        return inner + outer
    }
    
    /**
     * Resolves the given [name] to all symbols visible from the current scope, including all outer scopes. If [Type] is
     * specified more narrowly, then all symbols which are not of that type are omitted.
     */
    @JvmName("resolveType")
    inline fun <reified Type : HirSymbol> resolve(name: String): List<Type> =
        resolve(name).filterIsInstance<Type>()
    
    override fun toString(): String = _symbols.toString()
    override fun hashCode(): Int = _symbols.hashCode()
    override fun equals(other: Any?): Boolean = when (other)
    {
        is Scope -> _symbols == other._symbols
        else     -> false
    }
}
