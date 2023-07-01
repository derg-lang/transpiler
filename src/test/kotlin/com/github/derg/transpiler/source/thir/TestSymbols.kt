package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

/**
 * Helper function for binding a new arbitrary id under the given [name].
 */
private fun ThirSymbolTable.register(name: String): ThirSymbol = register(thirTypeOf(name))

class TestSymbolTable
{
    private val table = ThirSymbolTable()
    
    @Test
    fun `When binding identifier, then bound`()
    {
        val symbol = table.register("name")
        
        assertEquals(listOf(symbol), table.find("name"))
    }
    
    @Test
    fun `Given multiple identifiers, when finding, then found in reverse order`()
    {
        val symbol = listOf(table.register("name"), table.register("name")).reversed()
        
        assertEquals(symbol, table.find("name"))
    }
    
    @Test
    fun `Given no parent, when finding identifier, then correct outcome`()
    {
        val symbol = table.register("foo")
        
        assertEquals(listOf(symbol), table.find("foo"))
        assertEquals(emptyList<Id>(), table.find("bar"))
    }
    
    @Test
    fun `Given parent, when finding identifier, then correct outcome`()
    {
        val child = ThirSymbolTable(table)
        val symbolsFoo = listOf(table.register("foo"))
        val symbolsBar = listOf(child.register("bar"), table.register("bar"))
        
        assertEquals(symbolsFoo, child.find("foo"))
        assertEquals(symbolsBar, child.find("bar"))
        assertEquals(emptyList<Id>(), child.find("baz"))
    }
}
