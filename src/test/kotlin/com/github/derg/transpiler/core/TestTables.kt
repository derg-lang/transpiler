package com.github.derg.transpiler.core

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class TestSymbolTable
{
    private val table = SymbolTable()
    
    @Test
    fun `When registering name, then registered`()
    {
        val id = table.register("name")
        
        assertEquals(setOf(id), table.resolve("name"))
    }
    
    @Test
    fun `Given no name, when resolving name, then nothing resolved`()
    {
        assertEquals(emptySet<Id>(), table.resolve("name"))
    }
    
    @Test
    fun `Given multiple same names, when resolving name, then all resolved`()
    {
        val ids = setOf(
            table.register("name"),
            table.register("name"),
        )
        
        assertEquals(ids, table.resolve("name"))
    }
}
