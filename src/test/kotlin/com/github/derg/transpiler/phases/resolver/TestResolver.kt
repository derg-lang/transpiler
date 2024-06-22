package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.resolver.ResolveError.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import java.math.*

/**
 * Registers [this] symbol to the given [scope], returning the same symbol for convenience.
 */
private fun <Type : HirSymbol> Type.register(scope: Scope): Type = also { scope.register(it) }

class TestResolverSymbols
{
    private val types = TypeTable()
    private val symbols = SymbolTable()
    private val scope = Scope(Builtin.GLOBAL_SCOPE)
    
    private val resolver = Resolver(types, symbols, scope)
    
    @Nested
    inner class Fields
    {
        @Test
        fun `Given known type, when resolving, then resolved`()
        {
            val input = hirFieldOf(type = Builtin.BOOL_TYPE)
            val expected = input.toThir(type = thirTypeData(symbolId = Builtin.BOOL.id))
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given unknown type, when resolving, then failure`()
        {
            val symbol = hirStructOf()
            val expected = UnknownType(symbol.name)
            val input = hirFieldOf(type = hirTypeData(name = symbol.name))
            
            assertFailure(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given value, when resolving, then resolved`()
        {
            val input = hirFieldOf(value = 0.hir)
            val expected = thirFieldOf(value = 0.thir).bind(input)
            
            assertSuccess(expected, resolver.resolve(input))
        }
    }
    
    @Nested
    inner class Structs
    {
        @Test
        fun `Given builtin, when resolving, then resolved`()
        {
            assertSuccess(Builtin.BOOL.toThir(), resolver.resolve(Builtin.BOOL))
            assertSuccess(Builtin.INT32.toThir(), resolver.resolve(Builtin.INT32))
            assertSuccess(Builtin.INT64.toThir(), resolver.resolve(Builtin.INT64))
        }
        
        @Test
        fun `Given custom, when resolving, then resolved`()
        {
            val input = hirStructOf(fields = emptyList())
            val expected = thirStructOf(fields = emptySet()).bind(input)
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given field with known type, when resolving, then resolved`()
        {
            val field = hirFieldOf(type = Builtin.BOOL_TYPE)
            val input = hirStructOf(fields = listOf(field))
            val expected = thirStructOf(fields = setOf(field.id)).bind(input)
            
            assertSuccess(expected, resolver.resolve(input))
        }
    }
}

class TestResolverTypes
{
    private val types = TypeTable()
    private val symbols = SymbolTable()
    private val scope = Scope(Builtin.GLOBAL_SCOPE)
    
    private val resolver = Resolver(types, symbols, scope)
    
    @Nested
    inner class Call
    {
        @Test
        fun `Given plain old nothing, when resolving, then resolved`()
        {
            val input = hirTypeCall(valueType = null, errorType = null, parameters = emptyList())
            val expected = thirTypeCall(valueType = null, errorType = null, parameters = emptyList())
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given known value type, when resolving, then resolved`()
        {
            val input = hirTypeCall(valueType = Builtin.BOOL_TYPE)
            val expected = thirTypeCall(valueType = thirTypeData(symbolId = Builtin.BOOL.id))
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given unknown value type, when resolving, then failure`()
        {
            val symbol = hirStructOf()
            val input = hirTypeCall(valueType = hirTypeData(name = symbol.name))
            val expected = UnknownType(symbol.name)
            
            assertFailure(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given known error type, when resolving, then resolved`()
        {
            val input = hirTypeCall(errorType = Builtin.BOOL_TYPE)
            val expected = thirTypeCall(errorType = thirTypeData(symbolId = Builtin.BOOL.id))
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given unknown error type, when resolving, then failure`()
        {
            val symbol = hirStructOf()
            val input = hirTypeCall(errorType = hirTypeData(name = symbol.name))
            val expected = UnknownType(symbol.name)
            
            assertFailure(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given known parameter type, when resolving, then resolved`()
        {
            val input = hirTypeCall(parameters = listOf("param" to Builtin.BOOL_TYPE))
            val expected = thirTypeCall(parameters = listOf("param" to thirTypeData(symbolId = Builtin.BOOL.id)))
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given unknown parameter type, when resolving, then failure`()
        {
            val symbol = hirStructOf()
            val input = hirTypeCall(parameters = listOf("param" to hirTypeData(name = symbol.name)))
            val expected = UnknownType(symbol.name)
            
            assertFailure(expected, resolver.resolve(input))
        }
    }
    
    @Nested
    inner class Data
    {
        @Test
        fun `Given builtin struct, when resolving, then resolved`()
        {
            val input = Builtin.BOOL_TYPE
            val expected = thirTypeData(symbolId = Builtin.BOOL.id)
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given custom struct, when resolving, then resolved`()
        {
            val symbol = hirStructOf()
            val input = hirTypeData(name = symbol.name)
            val expected = thirTypeData(symbolId = symbol.id)
            
            scope.register(symbol)
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given unknown struct, when resolving, then failure`()
        {
            val symbol = hirStructOf()
            val input = hirTypeData(name = symbol.name)
            val expected = UnknownType(symbol.name)
            
            assertFailure(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given ambiguous struct, when resolving, then failure`()
        {
            val input = hirTypeData()
            val expected = AmbiguousType(input.name)
            
            scope.register(hirStructOf(name = input.name))
            scope.register(hirStructOf(name = input.name))
            
            assertFailure(expected, resolver.resolve(input))
        }
    }
}

class TestResolverValues
{
    private val types = TypeTable()
    private val symbols = SymbolTable()
    private val scope = Scope(Builtin.GLOBAL_SCOPE)
    
    private val resolver = Resolver(types, symbols, scope)
    
    @Nested
    inner class Bool
    {
        @Test
        fun `Given builtin, when resolving, then resolved`()
        {
            assertSuccess(true.thir, resolver.resolve(true.hir))
            assertSuccess(false.thir, resolver.resolve(false.hir))
        }
    }
    
    @Nested
    inner class Call
    {
        @Test
        fun `Given plain function, when resolving, then resolved`()
        {
            val symbol = hirFunOf().register(scope)
            val input = symbol.hirCall()
            val expected = symbol.thirCall()
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given return function, when resolving, then resolved`()
        {
            val symbol = hirFunOf(valueType = Builtin.BOOL_TYPE).register(scope)
            val input = symbol.hirCall()
            val expected = symbol.thirCall(valueType = thirTypeData(Builtin.BOOL.id))
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given raise function, when resolving, then resolved`()
        {
            val symbol = hirFunOf(errorType = Builtin.BOOL_TYPE).register(scope)
            val input = symbol.hirCall()
            val expected = symbol.thirCall(errorType = thirTypeData(Builtin.BOOL.id))
            
            assertSuccess(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given parameter function, when resolving, then resolved`()
        {
            val symbol = hirFunOf(parameters = listOf(hirParamOf(type = Builtin.BOOL_TYPE))).register(scope)
            val input = symbol.hirCall(parameters = listOf(null to true.hir))
            val expected = symbol.thirCall(parameters = listOf(true.thir))
            
            assertSuccess(expected, resolver.resolve(input))
        }
    
        @Test
        fun `Given generic function, when resolving, then resolved`()
        {
            val symbol = hirFunOf(generics = listOf(hirGenOf(template = HirTemplate.Type))).register(scope)
            val input = symbol.hirCall(generics = listOf(null to Builtin.BOOL.hirLoad()))
            val expected = symbol.thirCall(parameters = listOf(true.thir))
        
            assertSuccess(expected, resolver.resolve(input))
        }
    }
    
    @Nested
    inner class Integer
    {
        @Test
        fun `Given builtin, when resolving, then resolved`()
        {
            assertSuccess(0.thir, resolver.resolve(0.hir))
            assertSuccess(0L.thir, resolver.resolve(0L.hir))
        }
        
        @Test
        @Disabled // TODO: This test is too complicated to write for the moment.
        fun `Given known literal, when resolving, then resolved`()
        {
//            val input32 = HirInteger(BigInteger.ONE, "32")
//            val input64 = HirInteger(BigInteger.ONE, "64")
//
//            val int32 = hirLitOf(name = input32.literal, type = Builtin.INT32_TYPE).register(scope)
//            val int64 = hirLitOf(name = input64.literal, type = Builtin.INT64_TYPE).register(scope)
//
//            assertSuccess(int32.thirCall(1), resolver.resolve(input32))
//            assertSuccess(int64.thirCall(1L), resolver.resolve(input64))
        }
        
        @Test
        fun `Given unknown literal, when resolving, then failure`()
        {
            val input = HirInteger(BigInteger.ONE, "literal")
            val expected = UnknownLiteral(input.literal)
            
            assertFailure(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given ambiguous literal, when resolving, then failure`()
        {
            val input = HirInteger(BigInteger.ONE, "literal")
            val expected = AmbiguousLiteral(input.literal, input)
            
            hirLitOf(name = input.literal, valueType = Builtin.INT32_TYPE).register(scope)
            hirLitOf(name = input.literal, valueType = Builtin.INT32_TYPE).register(scope)
            
            assertFailure(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given custom parameter call type, when resolving, then failure`()
        {
            val literal = hirLitOf(param = hirParamOf(type = hirTypeCall())).register(scope)
            val expected = InvalidType(name = literal.name, type = literal.parameter.type)
            val input = HirInteger(BigInteger.ONE, literal.name)
            
            assertFailure(expected, resolver.resolve(input))
        }
        
        @Test
        fun `Given custom parameter data type, when resolving, then failure`()
        {
            val struct = hirStructOf().register(scope)
            val literal = hirLitOf(param = hirParamOf(type = hirTypeData(name = struct.name))).register(scope)
            val expected = InvalidType(name = literal.name, type = literal.parameter.type)
            val input = HirInteger(BigInteger.ONE, literal.name)
            
            assertFailure(expected, resolver.resolve(input))
        }
    }
}
