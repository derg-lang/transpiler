package com.github.derg.transpiler.source.thir

import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestSymbolTable
{
    private val table = ThirSymbolTable()
    
    @Test
    fun `When binding identifier, then bound`()
    {
        val symbol = thirTypeOf("name").also(table::register)
        
        assertEquals(listOf(symbol), table["name"])
    }
    
    @Test
    fun `Given identifier in root, when resolving by id, then found`()
    {
        val child = ThirSymbolTable(table)
        val symbol = thirTypeOf("name").also(table::register)
        
        assertEquals(symbol, table[symbol.id])
        assertEquals(symbol, child[symbol.id])
    }
    
    @Test
    fun `Given identifier in child, when resolving by id, then found`()
    {
        val child = ThirSymbolTable(table)
        val symbol = thirTypeOf("name").also(child::register)
        
        assertEquals(symbol, table[symbol.id])
        assertEquals(symbol, child[symbol.id])
    }
    
    @Test
    fun `Given multiple identifiers, when finding, then found in reverse order`()
    {
        val symbol = listOf(thirTypeOf("name"), thirTypeOf("name")).onEach(table::register).reversed()
        
        assertEquals(symbol, table["name"])
    }
    
    @Test
    fun `Given no parent, when finding identifier, then correct outcome`()
    {
        val symbol = thirTypeOf("foo").also(table::register)
        
        assertEquals(listOf(symbol), table["foo"])
        assertEquals(emptyList<ThirId>(), table["bar"])
    }
    
    @Test
    fun `Given parent, when finding identifier, then correct outcome`()
    {
        val child = ThirSymbolTable(table)
        val symbolsFoo = listOf(thirTypeOf("foo").also(table::register))
        val symbolsBar = listOf(thirTypeOf("bar").also(child::register), thirTypeOf("bar").also(table::register))
        
        assertEquals(symbolsFoo, child["foo"])
        assertEquals(symbolsBar, child["bar"])
        assertEquals(emptyList<ThirId>(), child["baz"])
    }
}
