package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.parser.funOf
import com.github.derg.transpiler.phases.parser.parOf
import com.github.derg.transpiler.source.IdProviderNil
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.util.toFailure
import com.github.derg.transpiler.util.toSuccess
import com.github.derg.transpiler.util.valueOrDie
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class TestDeclaratorFunction
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorFunction(symbols, IdProviderNil)
    
    @Test
    fun `Given basic, when declaring, then registered`()
    {
        val symbol = declarator(funOf("function")).valueOrDie()
        
        assertEquals(listOf(symbol), symbols.find(symbol.name))
    }
    
    @Test
    fun `Given basic, when declaring, then correct outcome`()
    {
        val node = funOf("function")
        val expected = functionOf(node.name)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given valid return type, when declaring, then correct outcome`()
    {
        val node = funOf("function", valType = Builtin.INT32.name)
        val expected = functionOf(node.name, valueType = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid return type, when declaring, then correct error`()
    {
        val node = funOf("function", valType = "unknown")
        val expected = ResolveError.UnknownType("unknown")
        
        assertEquals(expected.toFailure(), declarator(node))
    }
    
    @Test
    fun `Given valid error type, when declaring, then correct outcome`()
    {
        val node = funOf("function", errType = Builtin.INT32.name)
        val expected = functionOf(node.name, errorType = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid error type, when declaring, then correct error`()
    {
        val node = funOf("function", errType = "unknown")
        val expected = ResolveError.UnknownType("unknown")
        
        assertEquals(expected.toFailure(), declarator(node))
    }
    
    @Test
    fun `Given parameter, when declaring, then registered`()
    {
        val symbol = declarator(funOf("function", params = listOf(parOf("parameter")))).valueOrDie()
        
        assertEquals(listOf(symbol.params[0]), symbol.symbols.find(symbol.params[0].name))
    }
    
    @Test
    fun `Given parameter, when declaring, then correct outcome`()
    {
        val node = funOf("function", params = listOf(parOf("a", type = Builtin.INT32.name)))
        val expected = functionOf(node.name, params = listOf(parameterOf("a", type = Builtin.INT32)))
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
}

class TestDeclaratorParameter
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorParameter(symbols, IdProviderNil)
    
    @Test
    fun `Given basic, when declaring, then registered`()
    {
        val symbol = declarator(parOf("parameter")).valueOrDie()
        
        assertEquals(listOf(symbol), symbols.find(symbol.name))
    }
    
    @Test
    fun `Given valid type, when declaring, then registered`()
    {
        val node = parOf("parameter", type = Builtin.INT32.name)
        val expected = parameterOf(node.name, type = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given valid type, when declaring, then correct outcome`()
    {
        val node = parOf("parameter", type = Builtin.INT32.name)
        val expected = parameterOf(node.name, type = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid type, when declaring, then correct error`()
    {
        val node = parOf("parameter", type = "unknown")
        val expected = ResolveError.UnknownType("unknown")
        
        assertEquals(expected.toFailure(), declarator(node))
    }
    
    @Test
    fun `Given valid value, when declaring, then correct outcome`()
    {
        val node = parOf("parameter", type = Builtin.INT32.name, value = 1.e)
        val expected = parameterOf(node.name, type = Builtin.INT32, value = 1.v)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid value, when declaring, then correct outcome`()
    {
        val node = parOf("parameter", type = Builtin.INT32.name, value = true.e)
        val expected = ResolveError.MismatchedParameterType(expected = Builtin.INT32, actual = Builtin.BOOL)
        
        assertEquals(expected.toFailure(), declarator(node))
    }
}
