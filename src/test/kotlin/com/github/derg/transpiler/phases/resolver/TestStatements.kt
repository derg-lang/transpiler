package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Assignment
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.util.toFailure
import com.github.derg.transpiler.util.toSuccess
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class TestConverterAssign
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterAssign(symbols)
    
    private val variable = hirVarOf("foo", type = Builtin.INT32).also { symbols.register(it) }
    
    @Test
    fun `Given valid value, when resolving, then correct outcome`()
    {
        assertEquals(Assign(variable, 1.v).toSuccess(), converter(Assignment.Assign(variable.name, 1.e)))
    }
    
    @Test
    fun `Given invalid value, when resolving, then correct error`()
    {
        val expected = ResolveError.MismatchedVariableType(expected = variable.type, actual = Builtin.BOOL)
        
        assertEquals(expected.toFailure(), converter(Assignment.Assign(variable.name, true.e)))
    }
    
    @Test
    fun `Given unknown variable, when resolving, then correct error`()
    {
        val expected = ResolveError.UnknownVariable("unknown")
        
        assertEquals(expected.toFailure(), converter(Assignment.Assign("unknown", 1.e)))
    }
}
