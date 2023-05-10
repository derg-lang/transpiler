package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Control
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.util.toSuccess
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class TestConverterStatements
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterStatements(symbols)
    
    @Test
    fun `Given raise statement, when resolving, then correctly resolved`()
    {
        assertEquals(Raise(42.v).toSuccess(), converter.convert(Control.Raise(42.e)))
    }
    
    @Test
    fun `Given return statement, when resolving, then correctly resolved`()
    {
        assertEquals(Exit.toSuccess(), converter.convert(Control.Return(null)))
        assertEquals(Return(42.v).toSuccess(), converter.convert(Control.Return(42.e)))
    }
}
