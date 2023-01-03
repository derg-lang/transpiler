package com.github.derg.transpiler.source.lir

import com.github.derg.transpiler.source.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows

class TestSymbolTable
{
    private val table = SymbolTable()
    private val symbol = Module(IdModule(0))
    
    @Test
    fun `Given unknown id, when resolving symbol, then not found`()
    {
        assertNull(table.resolve(symbol.id))
    }
    
    @Test
    fun `Given known id, when resolving symbol, then found`()
    {
        table.register(symbol)
        
        assertEquals(symbol, table.resolve(symbol.id))
    }
    
    @Test
    fun `Given registered symbol, when registering symbol again, then error`()
    {
        table.register(symbol)
        
        assertThrows<IllegalArgumentException> { table.register(symbol) }
    }
}

class TestTranslationTable
{
    private val table = TranslationTable()
    
    @Test
    fun `When registering name, then registered`()
    {
        val id = table.register("name", ::IdVariable)
        
        assertEquals(listOf(id), table.resolve("name"))
    }
    
    @Test
    fun `Given no name, when resolving name, then nothing resolved`()
    {
        assertEquals(emptyList<Id>(), table.resolve("name"))
    }
    
    @Test
    fun `Given multiple same names, when resolving name, then all resolved`()
    {
        val ids = listOf(
            table.register("name", ::IdType),
            table.register("name", ::IdType),
            table.register("name", ::IdParameter),
        )
        
        assertEquals(ids, table.resolve("name"))
    }
}
