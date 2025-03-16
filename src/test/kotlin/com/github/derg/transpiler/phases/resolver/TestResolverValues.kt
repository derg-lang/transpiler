package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.resolver.ResolveError.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import java.math.*
import java.util.*

/**
 * Generates a thir-representation of the struct.
 */
private fun HirStruct.asThir() = ThirType.Structure(
    symbolId = id,
    mutability = Mutability.IMMUTABLE,
    parameters = emptyList(),
)

/**
 * Generates a thir-representation of the value.
 */
private fun HirValue.asThir(): ThirValue = when (this)
{
    is HirInteger -> when (literal)
    {
        INT32_LIT_NAME -> ThirConstInt32(value.toInt())
        INT64_LIT_NAME -> ThirConstInt64(value.toLong())
        else           -> throw IllegalStateException("Integer of type '$literal' is not supported in these tests")
    }
    else          -> throw IllegalStateException("Not supported in the unit tests")
}

/**
 * Simulates what a thir call on [this] function would be given the list of [parameters]. The function is assumed
 * to have no return value or error.
 */
private fun HirFunction.thirCall(vararg parameters: Any): ThirValue
{
    val inputs = parameters.map { it.thir }
    val params = this.parameters.zip(inputs).map { thirTypeParam(name = it.first.name, type = it.second.value!!, value = it.first.value?.asThir()) }
    val type = ThirType.Function(null, null, params)
    
    return ThirCall(null, null, ThirLoad(type, id, emptyList()), inputs)
}

private fun HirLiteral.thirCall(parameter: Any): ThirValue
{
    val input = parameter.thir
    val output = ThirType.Structure(Builtin.INT32.id, Mutability.IMMUTABLE, emptyList())
    val type = ThirType.Function(output, null, listOf(thirTypeParam(name = "", type = input.value!!, value = this.parameter.value?.asThir())))
    
    return ThirCall(output, null, ThirLoad(type, id, emptyList()), listOf(input))
}

class TestResolverValue
{
    private val scope = Scope(Builtin.GLOBAL_SCOPE)
    private val engine = ResolutionEngine()
    
    /**
     * Generates and registers a new HIR function with the given [name], where the [parameters] are given the specified
     * types.
     */
    private fun registerFun(name: String, vararg parameters: HirType): HirFunction =
        hirFunOf(name = name, params = parameters.map { hirParamOf(type = it) }).also(scope::register)
    
    private fun registerLit(name: String, parameter: HirType): HirLiteral =
        hirLitOf(name = name, param = hirParamOf(name = "", type = parameter)).also(scope::register)
    
    private fun registerVar(name: String): HirVariable =
        hirVarOf(name = name).also(scope::register)
    
    private fun registerParam(name: String): HirParameter =
        hirParamOf(name = name).also(scope::register)
    
    private fun registerStruct(name: String): HirStruct =
        hirStructOf(name = name).also(scope::register)
    
    /**
     * Converts the [value] if possible, ensuring that all global scopes are registered into the engine before
     * performing the test.
     */
    private fun run(value: HirValue): Result<ThirValue, ResolveError>
    {
        engine.prepare(Builtin.GLOBAL_SCOPE).onFailure { return it.toFailure() }
        engine.prepare(scope).onFailure { return it.toFailure() }
        
        return engine.resolve(scope, value)
    }
    
    @Nested
    inner class Add
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirAdd 2, run(1 hirAdd 2))
            assertSuccess(1L thirAdd 2L, run(1L hirAdd 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.PLUS.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirAdd 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.PLUS.symbol, listOf(null hirArg true, null hirArg false))
            
            assertFailure(expected, run(true hirAdd false))
        }
    }
    
    @Nested
    inner class And
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirAnd false, run(true hirAnd false))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.AND.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirAnd 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.AND.symbol, listOf(null hirArg false, null hirArg 0))
            
            assertFailure(expected, run(false hirAnd 0))
        }
    }
    
    @Nested
    inner class Bool
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true.thir, run(true.hir))
        }
    }
    
    @Nested
    inner class Call
    {
        @Test
        fun `Given no parameters, when resolving, then correct outcome`()
        {
            val function = registerFun("fun")
            
            assertSuccess(function.thirCall(), run(function.hirLoad().hirCall()))
        }
        
        @Test
        fun `Given some parameters, when resolving, then correct outcome`()
        {
            val function = registerFun("fun", Builtin.INT32_TYPE)
            
            assertSuccess(function.thirCall(1), run(function.hirLoad().hirCall(null to 1)))
        }
        
        @Test
        fun `Given mismatching parameter, when resolving, then correct error`()
        {
            val function = registerFun("fun", Builtin.INT32_TYPE)
            val expected = ArgumentMismatch(function.name, listOf(null hirArg true))
            
            assertFailure(expected, run(function.hirLoad().hirCall(null to true)))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val instance = HirLoad("unknown", emptyList())
            val expected = UnknownFunction("unknown")
            
            assertFailure(expected, run(HirCall(instance, emptyList())))
        }
        
        @Test
        fun `Given ambiguous overload, when resolving, then correct error`()
        {
            val functions = listOf(
                registerFun("fun", Builtin.BOOL_TYPE),
                registerFun("fun", Builtin.BOOL_TYPE),
            )
            val expected = AmbiguousFunction(functions[0].name, listOf(null hirArg true))
            
            assertFailure(expected, run(functions[0].hirLoad().hirCall(null to true)))
        }
        
        @Test
        fun `Given named parameters, when resolving, then correct outcome`()
        {
            val params = listOf(hirParamOf("a"), hirParamOf("b"))
            val function = hirFunOf(params = params).also { scope.register(it) }
            
            assertSuccess(function.thirCall(1, 2), run(function.hirLoad().hirCall(null to 1, null to 2)))
            assertSuccess(function.thirCall(1, 2), run(function.hirLoad().hirCall(null to 1, "b" to 2)))
            assertSuccess(function.thirCall(1, 2), run(function.hirLoad().hirCall("a" to 1, "b" to 2)))
            assertSuccess(function.thirCall(1, 2), run(function.hirLoad().hirCall("b" to 2, "a" to 1)))
        }
        
        @Test
        fun `Given named parameters before unnamed, when resolving, then correct error`()
        {
            val params = listOf(hirParamOf("a"), hirParamOf("b"))
            val function = hirFunOf(params = params).also { scope.register(it) }
            val expected = ArgumentMisnamed(function.name, listOf("a" hirArg 1, null hirArg 2))
            
            assertFailure(expected, run(function.hirLoad().hirCall("a" to 1, null to 2)))
        }
        
        @Test
        fun `Given omitted parameter with default value, when resolving, then correct outcome`()
        {
            val params = listOf(hirParamOf(name = "a", value = 1.hir), hirParamOf(name = "b", value = 2.hir))
            val function = hirFunOf(params = params).also { scope.register(it) }
            
            assertSuccess(function.thirCall(1, 2), run(function.hirLoad().hirCall()))
            assertSuccess(function.thirCall(5, 2), run(function.hirLoad().hirCall(null to 5)))
            assertSuccess(function.thirCall(5, 2), run(function.hirLoad().hirCall("a" to 5)))
            assertSuccess(function.thirCall(1, 5), run(function.hirLoad().hirCall("b" to 5)))
        }
    }
    
    @Nested
    inner class Catch
    {
        @Test
        fun `Given handle, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirCatchHandle 2, run(1 hirCatchHandle 2))
        }
        
        @Test
        fun `Given raise, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirCatchRaise 2, run(1 hirCatchRaise 2))
        }
        
        @Test
        fun `Given return, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirCatchReturn 2, run(1 hirCatchReturn 2))
        }
    }
    
    @Nested
    inner class Divide
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirDiv 2, run(1 hirDiv 2))
            assertSuccess(1L thirDiv 2L, run(1L hirDiv 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.DIVIDE.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirDiv 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.DIVIDE.symbol, listOf(null hirArg true, null hirArg false))
            
            assertFailure(expected, run(true hirDiv false))
        }
    }
    
    @Nested
    inner class Equal
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirEq false, run(true hirEq false))
            assertSuccess(1 thirEq 2, run(1 hirEq 2))
            assertSuccess(1L thirEq 2L, run(1L hirEq 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.EQUAL.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirEq 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.EQUAL.symbol, listOf(null hirArg true, null hirArg 1))
            
            assertFailure(expected, run(true hirEq 1))
        }
    }
    
    @Nested
    inner class GreaterThan
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirGt 2, run(1 hirGt 2))
            assertSuccess(1L thirGt 2L, run(1L hirGt 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.GREATER.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirGt 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.GREATER.symbol, listOf(null hirArg true, null hirArg false))
            
            assertFailure(expected, run(true hirGt false))
        }
    }
    
    @Nested
    inner class GreaterEqual
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirGe 2, run(1 hirGe 2))
            assertSuccess(1L thirGe 2L, run(1L hirGe 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.GREATER_EQUAL.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirGe 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.GREATER_EQUAL.symbol, listOf(null hirArg true, null hirArg false))
            
            assertFailure(expected, run(true hirGe false))
        }
    }
    
    @Nested
    inner class Integer
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            // Int32
            assertSuccess(0.thir, run(0.hir))
            assertSuccess(1.thir, run(1.hir))
            assertSuccess(Int.MIN_VALUE.thir, run(Int.MIN_VALUE.hir))
            assertSuccess(Int.MAX_VALUE.thir, run(Int.MAX_VALUE.hir))
            assertFailure(InvalidLiteralInteger(INT32_MIN - 1), run(HirInteger(INT32_MIN - 1, INT32_LIT_NAME)))
            assertFailure(InvalidLiteralInteger(INT32_MAX + 1), run(HirInteger(INT32_MAX + 1, INT32_LIT_NAME)))
            
            // Int64
            assertSuccess(0L.thir, run(0L.hir))
            assertSuccess(1L.thir, run(1L.hir))
            assertSuccess(Long.MIN_VALUE.thir, run(Long.MIN_VALUE.hir))
            assertSuccess(Long.MAX_VALUE.thir, run(Long.MAX_VALUE.hir))
            assertFailure(InvalidLiteralInteger(INT64_MIN - 1), run(HirInteger(INT64_MIN - 1, INT64_LIT_NAME)))
            assertFailure(InvalidLiteralInteger(INT64_MAX + 1), run(HirInteger(INT64_MAX + 1, INT64_LIT_NAME)))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val int32 = registerLit("foo", Builtin.INT32_TYPE)
            val int64 = registerLit("bar", Builtin.INT64_TYPE)
            
            assertSuccess(int32.thirCall(1), run(HirInteger(BigInteger.ONE, int32.name)))
            assertSuccess(int64.thirCall(1L), run(HirInteger(BigInteger.ONE, int64.name)))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val value = HirInteger(BigInteger.ONE, "foo")
            
            assertFailure(UnknownLiteral("foo"), run(value))
        }
        
        @Test
        fun `Given ambiguous overload, when resolving, then correct error`()
        {
            registerLit("foo", Builtin.INT32_TYPE)
            registerLit("foo", Builtin.INT64_TYPE)
            
            val value = HirInteger(BigInteger.ONE, "foo")
            
            assertFailure(AmbiguousLiteral("foo", value), run(value))
        }
    }
    
    @Nested
    inner class LessThan
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirLt 2, run(1 hirLt 2))
            assertSuccess(1L thirLt 2L, run(1L hirLt 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.LESS.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirLt 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.LESS.symbol, listOf(null hirArg true, null hirArg false))
            
            assertFailure(expected, run(true hirLt false))
        }
    }
    
    @Nested
    inner class LessEqual
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirLe 2, run(1 hirLe 2))
            assertSuccess(1L thirLe 2L, run(1L hirLe 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.LESS_EQUAL.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirLe 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.LESS_EQUAL.symbol, listOf(null hirArg true, null hirArg false))
            
            assertFailure(expected, run(true hirLe false))
        }
    }
    
    @Nested
    inner class Load
    {
        @Test
        fun `Given function, when resolving, then correct error`()
        {
            val symbol = registerFun("foo")
            val expected = UnknownVariable(symbol.name)
            
            assertFailure(expected, run(symbol.hirLoad()))
        }
        
        @Test
        fun `Given variable, when resolving, then correct outcome`()
        {
            val symbol = registerVar("foo")
            val expected = ThirLoad(value = Builtin.INT32.asThir(), symbolId = symbol.id, parameters = emptyList())
            
            assertSuccess(expected, run(symbol.hirLoad()))
        }
    
        @Test
        fun `Given parameter, when resolving, then correct outcome`()
        {
            val symbol = registerParam("foo")
            val expected = ThirLoad(value = Builtin.INT32.asThir(), symbolId = symbol.id, parameters = emptyList())
            
            assertSuccess(expected, run(symbol.hirLoad()))
        }
    }
    
    @Nested
    inner class Member
    {
        private val field = hirFieldOf(type = Builtin.BOOL_TYPE).also(scope::register)
        private val struct = hirStructOf(fields = listOf(field)).also(scope::register)
        private val factory = hirFunOf(value = hirTypeData(struct)).also(scope::register)
        
        @Test
        fun `Given instance of struct, when accessing member, then correct outcome`()
        {
            engine.prepare(scope).valueOrDie()
            engine.resolve(scope, struct).valueOrDie()
            
            val type = thirTypeFun(value = struct.asThir())
            val instance = ThirLoad(value = type, symbolId = factory.id, parameters = emptyList())
            val call = ThirCall(value = struct.asThir(), error = null, instance = instance, parameters = emptyList())
            
            val input = factory.hirLoad().hirCall().hirMember(field.hirLoad())
            val expected = ThirMember(Builtin.BOOL.asThir(), call, field.id)
            
            assertSuccess(expected, run(input))
        }
    }
    
    @Nested
    inner class Minus
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1.thirMinus, run(1.hirMinus))
            assertSuccess(1L.thirMinus, run(1L.hirMinus))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.MINUS.symbol, Builtin.BOOL_TYPE)
            
            assertSuccess(function.thirCall(true), run(true.hirMinus))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.MINUS.symbol, listOf(null hirArg true))
            
            assertFailure(expected, run(true.hirMinus))
        }
    }
    
    @Nested
    inner class Modulo
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirMod 2, run(1 hirMod 2))
            assertSuccess(1L thirMod 2L, run(1L hirMod 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.MODULO.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirMod 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.MODULO.symbol, listOf(null hirArg true, null hirArg false))
            
            assertFailure(expected, run(true hirMod false))
        }
    }
    
    @Nested
    inner class Multiply
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirMul 2, run(1 hirMul 2))
            assertSuccess(1L thirMul 2L, run(1L hirMul 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.MULTIPLY.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirMul 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.MULTIPLY.symbol, listOf(null hirArg true, null hirArg false))
            
            assertFailure(expected, run(true hirMul false))
        }
    }
    
    @Nested
    inner class Not
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true.thirNot, run(true.hirNot))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.NOT.symbol, Builtin.INT32_TYPE)
            
            assertSuccess(function.thirCall(1), run(1.hirNot))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.NOT.symbol, listOf(null hirArg 0))
            
            assertFailure(expected, run(0.hirNot))
        }
    }
    
    @Nested
    inner class NotEqual
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirNe false, run(true hirNe false))
            assertSuccess(1 thirNe 2, run(1 hirNe 2))
            assertSuccess(1L thirNe 2L, run(1L hirNe 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.NOT_EQUAL.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirNe 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.NOT_EQUAL.symbol, listOf(null hirArg true, null hirArg 1))
            
            assertFailure(expected, run(true hirNe 1))
        }
    }
    
    @Nested
    inner class Or
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirOr false, run(true hirOr false))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.OR.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirOr 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.OR.symbol, listOf(null hirArg true, null hirArg 1))
            
            assertFailure(expected, run(true hirOr 1))
        }
    }
    
    @Nested
    inner class Plus
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1.thirPlus, run(1.hirPlus))
            assertSuccess(1L.thirPlus, run(1L.hirPlus))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.PLUS.symbol, Builtin.BOOL_TYPE)
            
            assertSuccess(function.thirCall(true), run(true.hirPlus))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.PLUS.symbol, listOf(null hirArg true))
            
            assertFailure(expected, run(true.hirPlus))
        }
    }
    
    @Nested
    inner class Subtract
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(1 thirSub 2, run(1 hirSub 2))
            assertSuccess(1L thirSub 2L, run(1L hirSub 2L))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.MINUS.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirSub 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.MINUS.symbol, listOf(null hirArg true, null hirArg false))
            
            assertFailure(expected, run(true hirSub false))
        }
    }
    
    @Nested
    inner class Xor
    {
        @Test
        fun `Given builtin types, when resolving, then correct outcome`()
        {
            assertSuccess(true thirXor false, run(true hirXor false))
        }
        
        @Test
        fun `Given known overload, when resolving, then correct outcome`()
        {
            val function = registerFun(Symbol.XOR.symbol, Builtin.INT32_TYPE, Builtin.INT64_TYPE)
            
            assertSuccess(function.thirCall(1, 2L), run(1 hirXor 2L))
        }
        
        @Test
        fun `Given unknown overload, when resolving, then correct error`()
        {
            val expected = ArgumentMismatch(Symbol.XOR.symbol, listOf(null hirArg false, null hirArg 1))
            
            assertFailure(expected, run(false hirXor 1))
        }
    }
}
