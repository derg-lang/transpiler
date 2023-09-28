package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.resolver.ResolveError.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

/**
 * Helper function for generating an infix operator function, which returns a bool and takes in any number of type
 * parameters.
 */
private fun boolFunctionOf(parent: ThirSymbolTable, name: String, vararg parameters: ThirType) = thirFunOf(
    name = name,
    valType = Builtin.BOOL,
    params = parameters.mapIndexed { i, type -> thirParOf(name = "${i + 1}", type = type) },
).also(parent::register)

class TestConverterExpression
{
    private val symbols = ThirSymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterExpression(symbols)
    
    @Nested
    inner class And
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirAnd false, converter(true astAnd false))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.AND.symbol, listOf(false.thirArg, 0.thirArg))
            
            assertFailure(expected, converter(false astAnd 0))
        }
    }
    
    @Nested
    inner class Add
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirAdd 2, converter(1 astAdd 2))
            assertSuccess(1L thirAdd 2L, converter(1L astAdd 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.PLUS.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astAdd 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.PLUS.symbol, listOf(true.thirArg, false.thirArg))
            
            assertFailure(expected, converter(true astAdd false))
        }
    }
    
    @Nested
    inner class Bool
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true.thir, converter(true.ast))
        }
    }
    
    @Nested
    inner class Call
    {
        private val bool = thirFunOf(valType = Builtin.BOOL).also(symbols::register)
        private val int32 = thirFunOf(valType = Builtin.INT32).also(symbols::register)
        private val int64 = thirFunOf(valType = Builtin.INT64).also(symbols::register)
        
        private val f = boolFunctionOf(symbols, "fun", Builtin.INT32, Builtin.INT64)
        
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(bool.thirCall(), converter(bool.name.astCall()))
            assertSuccess(int32.thirCall(), converter(int32.name.astCall()))
            assertSuccess(int64.thirCall(), converter(int64.name.astCall()))
        }
        
        @Test
        fun `Given unknown function, when resolving, then correct error`()
        {
            val expected = UnknownFunction("unknown")
            
            assertFailure(expected, converter("unknown".astCall()))
        }
        
        @Test
        fun `Given invalid parameters, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(f.name, listOf(true.thirArg))
            
            assertFailure(expected, converter(f.name.astCall(true)))
        }
        
        @Test
        fun `Given named parameters, when resolving, then correct outcome`()
        {
            assertSuccess(f.thirCall(1, "2" to 2L), converter(f.name.astCall(1, "2" to 2L)))
            assertSuccess(f.thirCall("1" to 1, "2" to 2L), converter(f.name.astCall("1" to 1, "2" to 2L)))
            assertSuccess(f.thirCall("1" to 1, "2" to 2L), converter(f.name.astCall("2" to 2L, "1" to 1)))
        }
        
        @Test
        fun `Given named parameters before unnamed, when resolving, then correct error`()
        {
            val expected = ArgumentMisnamed(f.name, listOf(("2" to 2L).thirArg, 1.thirArg))
            
            assertFailure(expected, converter(f.name.astCall("2" to 2L, 1)))
        }
        
        @Test
        fun `Given multiple candidates, when resolving, then correct error`()
        {
            val expected = ArgumentAmbiguous("overloaded", listOf(true.thirArg))
            
            boolFunctionOf(symbols, "overloaded", Builtin.BOOL)
            boolFunctionOf(symbols, "overloaded", Builtin.BOOL)
            
            assertFailure(expected, converter("overloaded".astCall(true)))
        }
    }
    
    @Nested
    inner class Divide
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirDiv 2, converter(1 astDiv 2))
            assertSuccess(1L thirDiv 2L, converter(1L astDiv 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.DIVIDE.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astDiv 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.DIVIDE.symbol, listOf(true.thirArg, false.thirArg))
            
            assertFailure(expected, converter(true astDiv false))
        }
    }
    
    @Nested
    inner class Equal
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirEq false, converter(true astEq false))
            assertSuccess(1 thirEq 2, converter(1 astEq 2))
            assertSuccess(1L thirEq 2L, converter(1L astEq 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.EQUAL.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astEq 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.EQUAL.symbol, listOf(true.thirArg, 1.thirArg))
            
            assertFailure(expected, converter(true astEq 1))
        }
    }
    
    @Nested
    inner class Greater
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirGt 2, converter(AstGreater(1.ast, 2.ast)))
            assertSuccess(1L thirGt 2L, converter(AstGreater(1L.ast, 2L.ast)))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.GREATER.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1.ast astGt 2L.ast))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.GREATER.symbol, listOf(true.thirArg, false.thirArg))
            
            assertFailure(expected, converter(AstGreater(true.ast, false.ast)))
        }
    }
    
    @Nested
    inner class GreaterEqual
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirGe 2, converter(1 astGe 2))
            assertSuccess(1L thirGe 2L, converter(1L astGe 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.GREATER_EQUAL.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astGe 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.GREATER_EQUAL.symbol, listOf(true.thirArg, false.thirArg))
            
            assertFailure(expected, converter(true astGe false))
        }
    }
    
    @Nested
    inner class Less
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirLt 2, converter(1 astLt 2))
            assertSuccess(1L thirLt 2L, converter(1L astLt 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.LESS.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astLt 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.LESS.symbol, listOf(true.thirArg, false.thirArg))
            
            assertFailure(expected, converter(true astLt false))
        }
    }
    
    @Nested
    inner class LessEqual
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirLe 2, converter(1 astLe 2))
            assertSuccess(1L thirLe 2L, converter(1L astLe 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.LESS_EQUAL.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astLe 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.LESS_EQUAL.symbol, listOf(true.thirArg, false.thirArg))
            
            assertFailure(expected, converter(true astLe false))
        }
    }
    
    @Nested
    inner class Modulo
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirMod 2, converter(1 astMod 2))
            assertSuccess(1L thirMod 2L, converter(1L astMod 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.MODULO.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astMod 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.MODULO.symbol, listOf(true.thirArg, false.thirArg))
            
            assertFailure(expected, converter(true astMod false))
        }
    }
    
    @Nested
    inner class Multiply
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirMul 2, converter(1 astMul 2))
            assertSuccess(1L thirMul 2L, converter(1L astMul 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.MULTIPLY.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astMul 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.MULTIPLY.symbol, listOf(true.thirArg, false.thirArg))
            
            assertFailure(expected, converter(true astMul false))
        }
    }
    
    @Nested
    inner class Not
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true.thirNot, converter(true.astNot))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.NOT.symbol, listOf(0.thirArg))
            
            assertFailure(expected, converter(0.astNot))
        }
    }
    
    @Nested
    inner class NotEqual
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirNe false, converter(true astNe false))
            assertSuccess(1 thirNe 2, converter(1 astNe 2))
            assertSuccess(1L thirNe 2L, converter(1L astNe 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.NOT_EQUAL.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astNe 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.NOT_EQUAL.symbol, listOf(true.thirArg, 1.thirArg))
            
            assertFailure(expected, converter(true astNe 1))
        }
    }
    
    @Nested
    inner class Or
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirOr false, converter(true astOr false))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.OR.symbol, listOf(true.thirArg, 0.thirArg))
            
            assertFailure(expected, converter(true astOr 0))
        }
    }
    
    @Nested
    inner class Read
    {
        private val bool = thirVarOf(type = Builtin.BOOL).also(symbols::register)
        private val int32 = thirVarOf(type = Builtin.INT32).also(symbols::register)
        private val int64 = thirVarOf(type = Builtin.INT64).also(symbols::register)
        
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(bool.thirRead, converter(bool.name.astRead))
            assertSuccess(int32.thirRead, converter(int32.name.astRead))
            assertSuccess(int64.thirRead, converter(int64.name.astRead))
        }
        
        @Test
        fun `Given unknown variable, when resolving, then correct error`()
        {
            val expected = UnknownVariable("unknown")
            
            assertFailure(expected, converter("unknown".astRead))
        }
    
        @Test
        fun `Given parameter, when resolving, then correct outcome`()
        {
            val param = thirParOf(type = Builtin.INT32).also(symbols::register)
        
            assertSuccess(param.thirRead, converter(param.name.astRead))
        }
        
        @Test
        fun `Given shadow variable, when resolving, then correct outcome`()
        {
            val first = thirVarOf(name = "shadowed", type = Builtin.BOOL).also(symbols::register)
            val shadow = thirVarOf(name = first.name, type = Builtin.INT32).also(symbols::register)
            
            assertSuccess(shadow.thirRead, converter(first.name.astRead))
        }
    }
    
    @Nested
    inner class Real
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1.thir, converter(AstReal(1, Builtin.INT32_LIT.name)))
            assertSuccess(2L.thir, converter(AstReal(2, Builtin.INT64_LIT.name)))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            // TODO: Implement me
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = UnknownLiteral("unknown")
            
            assertFailure(expected, converter(AstReal(1, "unknown")))
        }
    }
    
    @Nested
    inner class Subtract
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirSub 2, converter(1 astSub 2))
            assertSuccess(1L thirSub 2L, converter(1L astSub 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.MINUS.symbol, Builtin.INT32, Builtin.INT64)
            
            assertSuccess(function.thirCall(1, 2L), converter(1 astSub 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.MINUS.symbol, listOf(true.thirArg, false.thirArg))
            
            assertFailure(expected, converter(true astSub false))
        }
    }
    
    @Nested
    inner class Text
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            // TODO: Implement me
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            // TODO: Implement me
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = UnknownLiteral("unknown")
            
            assertFailure(expected, converter(AstText("", "unknown")))
        }
    }
    
    @Nested
    inner class UnaryMinus
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1.thirMinus, converter(1.astMinus))
            assertSuccess(1L.thirMinus, converter(1L.astMinus))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.MINUS.symbol, Builtin.BOOL)
            
            assertSuccess(function.thirCall(true), converter(true.astMinus))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.MINUS.symbol, listOf(true.thirArg))
            
            assertFailure(expected, converter(true.astMinus))
        }
    }
    
    @Nested
    inner class UnaryPlus
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1.thir, converter(1.astPlus))
            assertSuccess(1L.thir, converter(1L.astPlus))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = boolFunctionOf(symbols, SymbolType.PLUS.symbol, Builtin.BOOL)
            
            assertSuccess(function.thirCall(true), converter(true.astPlus))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.PLUS.symbol, listOf(true.thirArg))
            
            assertFailure(expected, converter(true.astPlus))
        }
    }
    
    @Nested
    inner class Xor
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirXor false, converter(true astXor false))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(SymbolType.XOR.symbol, listOf(false.thirArg, 0.thirArg))
            
            assertFailure(expected, converter(false astXor 0))
        }
    }
}
