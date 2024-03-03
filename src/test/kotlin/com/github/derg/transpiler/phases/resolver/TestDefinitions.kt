package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestConverterDefinitions
{
    private val symbols = ThirSymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterDefinitions(symbols)
    
    /**
     * Helper function for converting the given ast [node] into a specific thir symbol [Type].
     */
    private inline fun <reified Type : ThirSymbol> convert(node: AstSymbol): Type
    {
        assertSuccess(Unit, converter(listOf(node)))
        return symbols[node.name].filterIsInstance<Type>().single()
    }
    
    @Nested
    inner class Functions
    {
        @Test
        fun `Given no parameters, when declaring, then symbol is registered`()
        {
            val node = astFunOf(params = emptyList())
            
            assertSuccess(Unit, converter(listOf(node)))
            assertEquals(1, symbols[node.name].size)
        }
        
        @Test
        fun `Given parameter with valid type, when declaring, then parameter is registered in function's symbol table`()
        {
            val param = astParOf("a", Builtin.INT32.name)
            val symbol = convert<ThirFunction>(astFunOf(params = listOf(param)))
            
            assertEquals(0, symbols[param.name].size)
            assertEquals(1, symbol.scope.symbols[param.name].size)
        }
        
        @Test
        fun `Given parameter with unknown type, when declaring, then correct error`()
        {
            val node = astFunOf(params = listOf(astParOf("a", "unknown")))
            val expected = ResolveError.UnknownType("unknown")
            
            assertFailure(expected, converter(listOf(node)))
        }
        
        @Test
        fun `Given parameter with invalid type, when declaring, then correct error`()
        {
            val node = astFunOf(params = listOf(astParOf("a", Builtin.INT32.name, value = true)))
            val expected = ResolveError.MismatchedParameterType(Builtin.INT32.id, Builtin.BOOL.id)
            
            assertFailure(expected, converter(listOf(node)))
        }
        
        @Test
        fun `Given variables, when declaring, then variable is registered in function's symbol table`()
        {
            val variable = astVarOf()
            val symbol = convert<ThirFunction>(astFunOf(statements = listOf(variable)))
            
            assertEquals(0, symbols[variable.name].size)
            assertEquals(1, symbol.scope.symbols[variable.name].size)
        }
        
        @Test
        fun `Given no value type, when resolving, then correct outcome`()
        {
            val actual = convert<ThirFunction>(astFunOf(valType = null))
            val expected = thirFunOf(valType = Builtin.VOID).copy(id = actual.id, name = actual.name)
            
            assertEquals(expected, actual)
        }
        
        @Test
        fun `Given valid value type, when resolving, then correct outcome`()
        {
            val actual = convert<ThirFunction>(astFunOf(valType = Builtin.INT32.name))
            val expected = thirFunOf(valType = Builtin.INT32).copy(id = actual.id, name = actual.name)
            
            assertEquals(expected, actual)
        }
        
        @Test
        fun `Given invalid value type, when resolving, then correct error`()
        {
            val node = astFunOf(valType = Builtin.INT32.name, statements = listOf(true.astReturnValue))
            val expected = ResolveError.MismatchedReturnType(Builtin.INT32.id, Builtin.BOOL.id)
            
            assertFailure(expected, converter(listOf(node)))
        }
        
        @Test
        fun `Given unknown value type, when resolving, then correct error`()
        {
            val node = astFunOf(valType = "unknown")
            val expected = ResolveError.UnknownType("unknown")
            
            assertFailure(expected, converter(listOf(node)))
        }
        
        // TODO: Still lots of work needed before this test makes sense to add - need union types first
        @Test
        @Disabled
        fun `Given auto value, when resolving, then correct outcome`()
        {
            val actual = convert<ThirFunction>(astFunOf(valType = "auto", statements = listOf(1.astReturnValue)))
            val expected = thirFunOf(valType = Builtin.INT32).copy(id = actual.id, name = actual.name)
            
            assertEquals(expected, actual)
        }
        
        @Test
        fun `Given no error type, when resolving, then correct outcome`()
        {
            val actual = convert<ThirFunction>(astFunOf(errType = null))
            val expected = thirFunOf(errType = Builtin.VOID).copy(id = actual.id, name = actual.name)
            
            assertEquals(expected, actual)
        }
        
        @Test
        fun `Given valid error type, when resolving, then correct outcome`()
        {
            val actual = convert<ThirFunction>(astFunOf(errType = Builtin.INT32.name))
            val expected = thirFunOf(errType = Builtin.INT32).copy(id = actual.id, name = actual.name)
            
            assertEquals(expected, actual)
        }
        
        @Test
        fun `Given invalid error type, when resolving, then correct error`()
        {
            val node = astFunOf(errType = Builtin.INT32.name, statements = listOf(true.astReturnError))
            val expected = ResolveError.MismatchedReturnType(Builtin.INT32.id, Builtin.BOOL.id)
            
            assertFailure(expected, converter(listOf(node)))
        }
        
        @Test
        fun `Given unknown error type, when resolving, then correct error`()
        {
            val node = astFunOf(errType = "unknown")
            val expected = ResolveError.UnknownType("unknown")
            
            assertFailure(expected, converter(listOf(node)))
        }
        
        // TODO: Still lots of work needed before this test makes sense to add - need union types first
        @Test
        @Disabled
        fun `Given auto error, when resolving, then correct outcome`()
        {
            val actual = convert<ThirFunction>(astFunOf(errType = "auto", statements = listOf(1.astReturnError)))
            val expected = thirFunOf(valType = Builtin.INT32).copy(id = actual.id, name = actual.name)
            
            assertEquals(expected, actual)
        }
        
        @Test
        fun `Given return statement, when resolving, then correct outcome`()
        {
            val actual =
                convert<ThirFunction>(astFunOf(valType = Builtin.INT32.name, statements = listOf(0.astReturnValue)))
            val expected = thirFunOf(valType = Builtin.INT32).copy(id = actual.id, name = actual.name)
                .also { it.scope.instructions.add(ThirReturnValue(0.thir)) }
            
            assertEquals(expected, actual)
        }
    }
    
    @Nested
    inner class Types
    {
        @Test
        fun `Given symbol, when declaring, then symbol is registered`()
        {
            val node = astStructOf()
            
            assertSuccess(Unit, converter(listOf(node)))
            assertEquals(1, symbols[node.name].size)
        }
        
        @Test
        fun `Given no properties, when resolving, then correct outcome`()
        {
            val actual = convert<ThirType>(astStructOf())
            val expected = thirTypeOf().copy(id = actual.id, name = actual.name)
            
            assertEquals(expected, actual)
        }
        
        @Test
        fun `Given property with valid type, when declaring, then property is registered in type's symbol table`()
        {
            val param = astPropOf(type = Builtin.INT32.name)
            val symbol = convert<ThirType>(astStructOf(props = listOf(param)))
            
            assertEquals(0, symbols[param.name].size)
            assertEquals(1, symbol.scope.symbols[param.name].size)
        }
        
        @Test
        fun `Given property with unknown type, when declaring, then correct error`()
        {
            val node = astStructOf(props = listOf(astPropOf(type = "unknown")))
            val expected = ResolveError.UnknownType("unknown")
            
            assertFailure(expected, converter(listOf(node)))
        }
        
        @Test
        fun `Given property with invalid type, when declaring, then correct error`()
        {
            val node = astStructOf(props = listOf(astPropOf(type = Builtin.INT32.name, value = true)))
            val expected = ResolveError.MismatchedParameterType(Builtin.INT32.id, Builtin.BOOL.id)
            
            assertFailure(expected, converter(listOf(node)))
        }
    }
    
    @Nested
    inner class Variables
    {
        @Test
        fun `Given symbol, when declaring, then symbol is registered`()
        {
            val node = astVarOf()
            
            assertSuccess(Unit, converter(listOf(node)))
            assertEquals(1, symbols[node.name].size)
        }
        
        @Test
        fun `Given no value type, when resolving, then correct outcome`()
        {
            val actual = convert<ThirVariable>(astVarOf(value = 0))
            val expected = thirVarOf(type = Builtin.INT32).copy(id = actual.id, name = actual.name)
            
            assertEquals(expected, actual)
        }
        
        @Test
        fun `Given valid value type, when resolving, then correct outcome`()
        {
            val actual = convert<ThirVariable>(astVarOf(value = 0, type = Builtin.INT32.name))
            val expected = thirVarOf(type = Builtin.INT32).copy(id = actual.id, name = actual.name)
            
            assertEquals(expected, actual)
        }
        
        @Test
        fun `Given invalid value type, when resolving, then correct error`()
        {
            val node = astVarOf(value = 0, type = Builtin.BOOL.name)
            val expected = ResolveError.MismatchedVariableType(Builtin.BOOL.id, Builtin.INT32.id)
            
            assertFailure(expected, converter(listOf(node)))
        }
        
        @Test
        fun `Given unknown value type, when resolving, then correct error`()
        {
            val node = astVarOf(value = 0, type = "unknown")
            val expected = ResolveError.UnknownType("unknown")
            
            assertFailure(expected, converter(listOf(node)))
        }
        
        // TODO: Requires an expression for producing error values.
        @Test
        @Disabled
        fun `Given invalid error type, when resolving, then correct error`()
        {
            val node = astVarOf(value = 0)
            val expected = ResolveError.VariableWithError(0.thir)
            
            assertFailure(expected, converter(listOf(node)))
        }
    }
}
