package com.github.derg.transpiler.source.lir

import com.github.derg.transpiler.source.Id
import com.github.derg.transpiler.source.IdType
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class TestSymbolTable
{
    private val table = SymbolTable()
    
    @Test
    fun `When registering name, then registered`()
    {
        val id = table.register("name", ::IdType)
        
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
            table.register("name", ::IdType),
            table.register("name", ::IdType),
        )
        
        assertEquals(ids, table.resolve("name"))
    }
}
