package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.util.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestDeclarator
{
    private val symbols = ThirSymbolTable(Builtin.SYMBOLS)
    private val declarator = Declarator(symbols, IdProviderNil)
    
    @Test
    fun `Given function, when declaring, then registered`()
    {
        val node = astFunOf("name")
        
        assertTrue(declarator(listOf(node)).isSuccess)
        assertFalse(symbols.find(node.name).isEmpty())
    }
    
    @Test
    fun `Given type, when declaring, then registered`()
    {
        val node = astTypeOf("name")
        
        assertTrue(declarator(listOf(node)).isSuccess)
        assertFalse(symbols.find(node.name).isEmpty())
    }
    
    @Test
    fun `Given variable, when declaring, then registered`()
    {
        val node = astVarOf("name", 0)
        
        assertTrue(declarator(listOf(node)).isSuccess)
        assertFalse(symbols.find(node.name).isEmpty())
    }
}

class TestDeclaratorFunction
{
    private val symbols = ThirSymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorFunction(symbols, IdProviderNil)
    
    @Test
    fun `Given basic, when declaring, then correct outcome`()
    {
        val node = astFunOf("function")
        val expected = thirFunOf(node.name)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given valid return type, when declaring, then correct outcome`()
    {
        val node = astFunOf("function", valType = Builtin.INT32.name)
        val expected = thirFunOf(node.name, valueType = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid return type, when declaring, then correct error`()
    {
        val node = astFunOf("function", valType = "unknown")
        val expected = ResolveError.UnknownType("unknown")
        
        assertEquals(expected.toFailure(), declarator(node))
    }
    
    @Test
    fun `Given valid error type, when declaring, then correct outcome`()
    {
        val node = astFunOf("function", errType = Builtin.INT32.name)
        val expected = thirFunOf(node.name, errorType = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid error type, when declaring, then correct error`()
    {
        val node = astFunOf("function", errType = "unknown")
        val expected = ResolveError.UnknownType("unknown")
        
        assertEquals(expected.toFailure(), declarator(node))
    }
    
    @Test
    fun `Given parameter, when declaring, then correct outcome`()
    {
        val node = astFunOf("function", params = listOf(astParOf("a", type = Builtin.INT32.name)))
        val expected = thirFunOf(node.name, params = listOf(thirParOf("a", type = Builtin.INT32)))
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
}

class TestDeclaratorParameter
{
    private val symbols = ThirSymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorParameter(symbols, IdProviderNil)
    
    @Test
    fun `Given valid type, when declaring, then registered`()
    {
        val node = astParOf("parameter", type = Builtin.INT32.name)
        val expected = thirParOf(node.name, type = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given valid type, when declaring, then correct outcome`()
    {
        val node = astParOf("parameter", type = Builtin.INT32.name)
        val expected = thirParOf(node.name, type = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid type, when declaring, then correct error`()
    {
        val node = astParOf("parameter", type = "unknown")
        val expected = ResolveError.UnknownType("unknown")
        
        assertEquals(expected.toFailure(), declarator(node))
    }
    
    @Test
    fun `Given valid value, when declaring, then correct outcome`()
    {
        val node = astParOf("parameter", type = Builtin.INT32.name, value = 1.ast)
        val expected = thirParOf(node.name, type = Builtin.INT32, value = 1.thir)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid value, when declaring, then correct outcome`()
    {
        val node = astParOf("parameter", type = Builtin.INT32.name, value = true.ast)
        val expected = ResolveError.MismatchedParameterType(expected = Builtin.INT32, actual = Builtin.BOOL)
        
        assertEquals(expected.toFailure(), declarator(node))
    }
}

class TestDeclaratorType
{
    private val symbols = ThirSymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorType(symbols, IdProviderNil)
    
    @Test
    fun `Given basic, when declaring, then correct outcome`()
    {
        val node = astTypeOf("type")
        val expected = thirTypeOf(node.name)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
}

class TestDeclaratorVariable
{
    private val symbols = ThirSymbolTable(Builtin.SYMBOLS)
    private val declarator = DeclaratorVariable(symbols, IdProviderNil)
    
    @Test
    fun `Given basic, when declaring, then correct outcome`()
    {
        val node = astVarOf("variable", 1.ast)
        val expected = thirVarOf(node.name, type = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given valid type, when declaring, then correct outcome`()
    {
        val node = astVarOf("variable", 1.ast, type = Builtin.INT32.name)
        val expected = thirVarOf(node.name, type = Builtin.INT32)
        
        assertEquals(expected.toSuccess(), declarator(node))
    }
    
    @Test
    fun `Given invalid type, when declaring, then correct error`()
    {
        val node = astVarOf("variable", 1.ast, type = Builtin.BOOL.name)
        val expected = ResolveError.MismatchedVariableType(expected = Builtin.BOOL, actual = Builtin.INT32)
        
        assertEquals(expected.toFailure(), declarator(node))
    }
}
