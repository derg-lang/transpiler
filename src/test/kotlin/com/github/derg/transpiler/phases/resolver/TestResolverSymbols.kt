package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.resolver.ResolveError.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

/**
 * Generates a thir-representation of the struct.
 */
private fun HirStruct.asThir() = ThirTypeData(
    symbolId = id,
    generics = emptyList(),
    mutability = Mutability.IMMUTABLE,
)

/**
 * Generates a thir-representation of the parameter. Note that the type resolution is absolutely missing here!
 */
private fun HirParameter.asThir() = ThirParameter(
    id = id,
    name = name,
    type = Builtin.INT32.asThir(),
    value = null,
    passability = passability,
)

/**
 * Converts [this] to the expected symbol, inheriting the id and name from the [input] symbol.
 */
private fun ThirSymbol.toExpected(input: HirSymbol): ThirSymbol = when (this)
{
    is ThirFunction  -> copy(id = input.id, name = input.name)
    is ThirField     -> copy(id = input.id, name = input.name)
    is ThirParameter -> copy(id = input.id, name = input.name)
    is ThirStruct    -> copy(id = input.id, name = input.name)
    is ThirVariable  -> copy(id = input.id, name = input.name)
    else             -> throw IllegalArgumentException("Symbol '$this' is not supported")
}

class TestResolverSymbol
{
    private val scope = Scope(Builtin.GLOBAL_SCOPE)
    private val engine = ResolutionEngine()
    
    /**
     * Converts the [symbol] if possible, ensuring that all global scopes are registered into the engine before
     * performing the test.
     */
    private fun run(symbol: HirSymbol): Result<ThirSymbol, ResolveError>
    {
        scope.register(symbol)
        engine.prepare(Builtin.GLOBAL_SCOPE).onFailure { return it.toFailure() }
        engine.prepare(scope).onFailure { return it.toFailure() }
        
        return engine.resolve(scope, symbol)
    }
    
    @Nested
    inner class Field
    {
        @Test
        fun `Given type, when resolving, then correct outcome`()
        {
            val input = hirFieldOf(type = Builtin.INT32_TYPE)
            val expected = thirFieldOf(type = Builtin.INT32.asThir()).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given value, when resolving, then correct outcome`()
        {
            val input = hirFieldOf(value = 0.hir)
            val expected = thirFieldOf(value = 0.thir).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given self, when resolving, then symbol resolved`()
        {
            val input = hirFieldOf()
            
            run(input).valueOrDie()
            
            assertTrue(input.id in engine.symbols.fields)
        }
    }
    
    @Nested
    inner class Function
    {
        @Test
        fun `Given empty, when resolving, then correct outcome`()
        {
            val input = hirFunOf()
            val expected = thirFunOf().toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given value, when resolving, then correct outcome`()
        {
            val input = hirFunOf(value = Builtin.INT32_TYPE)
            val expected = thirFunOf(value = Builtin.INT32.asThir()).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given error, when resolving, then correct outcome`()
        {
            val input = hirFunOf(error = Builtin.INT32_TYPE)
            val expected = thirFunOf(error = Builtin.INT32.asThir()).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given parameter, when resolving, then correct outcome`()
        {
            val param = hirParamOf()
            val input = hirFunOf(params = listOf(param))
            val expected = thirFunOf(params = listOf(param.asThir())).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given self, when resolving, then symbol resolved`()
        {
            val input = hirFunOf()
            
            run(input).valueOrDie()
            
            assertTrue(input.id in engine.symbols.functions)
        }
        
        @Test
        fun `Given parameter, when resolving, then symbol resolved`()
        {
            val param = hirParamOf()
            val input = hirFunOf(params = listOf(param))
            
            run(input).valueOrDie()
            
            assertTrue(param.id in engine.symbols.parameters)
        }
    }
    
    @Nested
    inner class Parameter
    {
        @Test
        fun `Given type, when resolving, then correct outcome`()
        {
            val input = hirParamOf(type = Builtin.INT32_TYPE)
            val expected = thirParamOf(type = Builtin.INT32.asThir()).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
    
        @Test
        fun `Given unknown type, when resolving, then correct error`()
        {
            val input = hirParamOf(type = hirTypeData("invalid"))
            val expected = UnknownStruct("invalid")
        
            assertFailure(expected, run(input))
        }
    
        @Test
        fun `Given ambiguous type, when resolving, then correct error`()
        {
            val type = hirTypeData("ambiguous")
    
            hirStructOf(name = type.name).also { scope.register(it) }
            hirStructOf(name = type.name).also { scope.register(it) }
            
            val input = hirParamOf(type = type)
            val expected = AmbiguousStruct(type.name)
        
            assertFailure(expected, run(input))
        }
        
        @Test
        fun `Given value, when resolving, then correct outcome`()
        {
            val input = hirParamOf(value = 0.hir)
            val expected = thirParamOf(value = 0.thir).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given self, when resolving, then symbol resolved`()
        {
            val input = hirParamOf()
            
            run(input).valueOrDie()
            
            assertTrue(input.id in engine.symbols.parameters)
        }
    }
    
    @Nested
    inner class Struct
    {
        @Test
        fun `Given empty, when resolving, then correct outcome`()
        {
            val input = hirStructOf()
            val expected = thirStructOf().toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given field, when resolving, then correct outcome`()
        {
            val field = hirFieldOf()
            val input = hirStructOf(fields = listOf(field))
            val expected = thirStructOf(fields = setOf(field.id)).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given self, when resolving, then symbol resolved`()
        {
            val input = hirStructOf()
            
            run(input).valueOrDie()
            
            assertTrue(input.id in engine.symbols.structs)
        }
        
        @Test
        fun `Given field, when resolving, then symbol resolved`()
        {
            val field = hirFieldOf()
            val input = hirStructOf(fields = listOf(field))
            
            run(input).valueOrDie()
            
            assertTrue(field.id in engine.symbols.fields)
        }
    }
    
    @Nested
    inner class Variable
    {
        @Test
        fun `Given type, when resolving, then correct outcome`()
        {
            val input = hirVarOf(type = Builtin.INT32_TYPE)
            val expected = thirVarOf(type = Builtin.INT32.asThir()).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given unknown type, when resolving, then correct error`()
        {
            val input = hirVarOf(type = hirTypeData("invalid"))
            val expected = UnknownStruct("invalid")
            
            assertFailure(expected, run(input))
        }
    
        @Test
        fun `Given ambiguous type, when resolving, then correct error`()
        {
            val type = hirTypeData("ambiguous")
        
            hirStructOf(name = type.name).also { scope.register(it) }
            hirStructOf(name = type.name).also { scope.register(it) }
        
            val input = hirVarOf(type = type)
            val expected = AmbiguousStruct(type.name)
        
            assertFailure(expected, run(input))
        }
        
        @Test
        fun `Given value, when resolving, then correct outcome`()
        {
            val input = hirVarOf(value = 0.hir)
            val expected = thirVarOf(value = 0.thir).toExpected(input)
            
            assertSuccess(expected, run(input))
        }
        
        @Test
        fun `Given invalid value, when resolving, then correct error`()
        {
            val function = hirFunOf(value = null).also { scope.register(it) }
            
            val input = hirVarOf(value = function.hirCall())
            val expected = TypeMissing(input.name, input.value)
            
            assertFailure(expected, run(input))
        }
        
        @Test
        fun `Given self, when resolving, then symbol resolved`()
        {
            val input = hirVarOf()
            
            run(input).valueOrDie()
            
            assertTrue(input.id in engine.symbols.variables)
        }
    }
}
