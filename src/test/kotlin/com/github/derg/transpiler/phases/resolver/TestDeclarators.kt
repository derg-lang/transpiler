package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.parser.funOf
import com.github.derg.transpiler.phases.parser.parOf
import com.github.derg.transpiler.phases.parser.typeOf
import com.github.derg.transpiler.phases.parser.varOf
import com.github.derg.transpiler.source.IdProviderNil
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.util.isSuccess
import com.github.derg.transpiler.util.toFailure
import com.github.derg.transpiler.util.toSuccess
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class TestDeclarator
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val declarator = Declarator(symbols, IdProviderNil)
    
    @Test
    fun `Given function, when declaring, then registered`()
    {
        val node = funOf("name")
        
        assertTrue(declarator(listOf(node)).isSuccess)
        assertFalse(symbols.find(node.name).isEmpty())
    }
    
    @Test
    fun `Given type, when declaring, then registered`()
    {
        val node = typeOf("name")
        
        assertTrue(declarator(listOf(node)).isSuccess)
        assertFalse(symbols.find(node.name).isEmpty())
    }
    
    @Test
    fun `Given variable, when declaring, then registered`()
    {
        val node = varOf("name", 0)
        
        assertTrue(declarator(listOf(node)).isSuccess)
        assertFalse(symbols.find(node.name).isEmpty())
    }
}

class TestDeclaratorFunction
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorFunction(symbols, IdProviderNil)
    
    @Test
    fun `Given basic, when declaring, then correct outcome`()
    {
        val node = funOf("function")
        val expected = hirFunOf(node.name)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given valid return type, when declaring, then correct outcome`()
    {
        val node = funOf("function", valType = Builtin.INT32.name)
        val expected = hirFunOf(node.name, valueType = Builtin.INT32)
        
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
        val expected = hirFunOf(node.name, errorType = Builtin.INT32)
        
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
    fun `Given parameter, when declaring, then correct outcome`()
    {
        val node = funOf("function", params = listOf(parOf("a", type = Builtin.INT32.name)))
        val expected = hirFunOf(node.name, params = listOf(hirParOf("a", type = Builtin.INT32)))
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
}

class TestDeclaratorParameter
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorParameter(symbols, IdProviderNil)
    
    @Test
    fun `Given valid type, when declaring, then registered`()
    {
        val node = parOf("parameter", type = Builtin.INT32.name)
        val expected = hirParOf(node.name, type = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given valid type, when declaring, then correct outcome`()
    {
        val node = parOf("parameter", type = Builtin.INT32.name)
        val expected = hirParOf(node.name, type = Builtin.INT32)
        
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
        val expected = hirParOf(node.name, type = Builtin.INT32, value = 1.v)
        
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

class TestDeclaratorType
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorType(symbols, IdProviderNil)
    
    @Test
    fun `Given basic, when declaring, then correct outcome`()
    {
        val node = typeOf("type")
        val expected = hirTypeOf(node.name)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
}

class TestDeclaratorVariable
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorVariable(symbols, IdProviderNil)
    
    @Test
    fun `Given basic, when declaring, then correct outcome`()
    {
        val node = varOf("variable", 1.e)
        val expected = hirVarOf(node.name, type = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given valid type, when declaring, then correct outcome`()
    {
        val node = varOf("variable", 1.e, type = Builtin.INT32.name)
        val expected = hirVarOf(node.name, type = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid type, when declaring, then correct error`()
    {
        val node = varOf("variable", 1.e, type = Builtin.BOOL.name)
        val expected = ResolveError.MismatchedVariableType(expected = Builtin.BOOL, actual = Builtin.INT32)
        
        assertEquals(expected.toFailure(), declarator(node))
    }
}
