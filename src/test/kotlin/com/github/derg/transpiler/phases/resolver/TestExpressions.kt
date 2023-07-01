package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.parser.*
import com.github.derg.transpiler.phases.resolver.ResolveError.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.util.*
import org.junit.jupiter.api.Test
import kotlin.test.*

class TestConverterAnd
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterAnd(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolAnd(true.thir, false.thir).toSuccess(), converter(AstAnd(true.ast, false.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.AND.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(AstAnd(false.ast, 0.ast)))
    }
}

class TestConverterAdd
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterAdd(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.PLUS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Add(1.thir, 2.thir).toSuccess(), converter(AstAdd(1.ast, 2.ast)))
        assertEquals(Int64Add(1L.thir, 2L.thir).toSuccess(), converter(AstAdd(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstAdd(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.PLUS.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstAdd(true.ast, false.ast)))
    }
}

class TestConverterBool
{
    private val converter = ConverterBool
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolConst(true).toSuccess(), converter(AstBool(true)))
    }
}

class TestConverterCall
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterCall(symbols)
    
    private val bool = thirFunOf(name = "bool", valueType = Builtin.BOOL).also { symbols.register(it) }
    private val int32 = thirFunOf(name = "int32", valueType = Builtin.INT32).also { symbols.register(it) }
    private val int64 = thirFunOf(name = "int64", valueType = Builtin.INT64).also { symbols.register(it) }
    
    private val params = thirFunOf(
        name = "params",
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("foo", Builtin.INT32), thirParOf("bar", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(bool, emptyList()).toSuccess(), converter(AstCall(bool.name, emptyList())))
        assertEquals(Int32Call(int32, emptyList()).toSuccess(), converter(AstCall(int32.name, emptyList())))
        assertEquals(Int64Call(int64, emptyList()).toSuccess(), converter(AstCall(int64.name, emptyList())))
    }
    
    @Test
    fun `Given unknown function, when resolving, then correct error`()
    {
        val expected = UnknownFunction("unknown")
        
        assertEquals(expected.toFailure(), converter(AstCall("unknown", emptyList())))
    }
    
    @Test
    fun `Given valid parameters, when resolving, then correct outcome`()
    {
        val input = AstCall(params.name, listOf(1.toArg(), 2.toExp(Builtin.LIT_INT64).toArg()))
        val expected = BoolCall(params, listOf(1.thir, 2L.thir))
        
        assertEquals(expected.toSuccess(), converter(input))
    }
    
    @Test
    fun `Given invalid parameters, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(params.name, listOf(Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstCall(params.name, listOf(true.toArg()))))
    }
}

class TestConverterDivide
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterDivide(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.DIVIDE.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Div(1.thir, 2.thir).toSuccess(), converter(AstDivide(1.ast, 2.ast)))
        assertEquals(Int64Div(1L.thir, 2L.thir).toSuccess(), converter(AstDivide(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstDivide(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.DIVIDE.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstDivide(true.ast, false.ast)))
    }
}

class TestConverterEqual
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterEqual(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.EQUAL.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolEq(true.thir, false.thir).toSuccess(), converter(AstEqual(true.ast, false.ast)))
        assertEquals(Int32Eq(1.thir, 2.thir).toSuccess(), converter(AstEqual(1.ast, 2.ast)))
        assertEquals(Int64Eq(1L.thir, 2L.thir).toSuccess(), converter(AstEqual(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstEqual(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.EQUAL.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(AstEqual(true.ast, 1.ast)))
    }
}

class TestConverterGreater
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterGreater(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.GREATER.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Gt(1.thir, 2.thir).toSuccess(), converter(AstGreater(1.ast, 2.ast)))
        assertEquals(Int64Gt(1L.thir, 2L.thir).toSuccess(), converter(AstGreater(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstGreater(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.GREATER.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstGreater(true.ast, false.ast)))
    }
}

class TestConverterGreaterEqual
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterGreaterEqual(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.GREATER_EQUAL.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Ge(1.thir, 2.thir).toSuccess(), converter(AstGreaterEqual(1.ast, 2.ast)))
        assertEquals(Int64Ge(1L.thir, 2L.thir).toSuccess(), converter(AstGreaterEqual(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstGreaterEqual(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.GREATER_EQUAL.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstGreaterEqual(true.ast, false.ast)))
    }
}

class TestConverterLess
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterLess(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.LESS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Lt(1.thir, 2.thir).toSuccess(), converter(AstLess(1.ast, 2.ast)))
        assertEquals(Int64Lt(1L.thir, 2L.thir).toSuccess(), converter(AstLess(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstLess(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.LESS.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstLess(true.ast, false.ast)))
    }
}

class TestConverterLessEqual
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterLessEqual(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.LESS_EQUAL.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Le(1.thir, 2.thir).toSuccess(), converter(AstLessEqual(1.ast, 2.ast)))
        assertEquals(Int64Le(1L.thir, 2L.thir).toSuccess(), converter(AstLessEqual(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstLessEqual(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.LESS_EQUAL.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstLessEqual(true.ast, false.ast)))
    }
}

class TestConverterModulo
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterModulo(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.MODULO.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Mod(1.thir, 2.thir).toSuccess(), converter(AstModulo(1.ast, 2.ast)))
        assertEquals(Int64Mod(1L.thir, 2L.thir).toSuccess(), converter(AstModulo(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstModulo(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.MODULO.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstModulo(true.ast, false.ast)))
    }
}

class TestConverterMultiply
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterMultiply(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.MULTIPLY.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Mul(1.thir, 2.thir).toSuccess(), converter(AstMultiply(1.ast, 2.ast)))
        assertEquals(Int64Mul(1L.thir, 2L.thir).toSuccess(), converter(AstMultiply(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstMultiply(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.MULTIPLY.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstMultiply(true.ast, false.ast)))
    }
}

class TestConverterNot
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterNot(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolNot(true.thir).toSuccess(), converter(AstNot(true.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.NOT.symbol, listOf(Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(AstNot(0.ast)))
    }
}

class TestConverterNotEqual
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterNotEqual(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.NOT_EQUAL.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolNe(true.thir, false.thir).toSuccess(), converter(AstNotEqual(true.ast, false.ast)))
        assertEquals(Int32Ne(1.thir, 2.thir).toSuccess(), converter(AstNotEqual(1.ast, 2.ast)))
        assertEquals(Int64Ne(1L.thir, 2L.thir).toSuccess(), converter(AstNotEqual(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstNotEqual(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.NOT_EQUAL.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(AstNotEqual(true.ast, 1.ast)))
    }
}

class TestConverterOr
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterOr(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolOr(true.thir, false.thir).toSuccess(), converter(AstOr(true.ast, false.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.OR.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(AstOr(false.ast, 0.ast)))
    }
}

class TestConverterRead
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterRead(symbols)
    
    private val bool = thirVarOf(name = "bool", type = Builtin.BOOL).also { symbols.register(it) }
    private val int32 = thirVarOf(name = "int32", type = Builtin.INT32).also { symbols.register(it) }
    private val int64 = thirVarOf(name = "int64", type = Builtin.INT64).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolRead(bool).toSuccess(), converter(AstRead(bool.name)))
        assertEquals(Int32Read(int32).toSuccess(), converter(AstRead(int32.name)))
        assertEquals(Int64Read(int64).toSuccess(), converter(AstRead(int64.name)))
    }
    
    @Test
    fun `Given unknown variable, when resolving, then correct error`()
    {
        val expected = UnknownVariable("unknown")
        
        assertEquals(expected.toFailure(), converter(AstRead("unknown")))
    }
}

class TestConverterReal
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterReal(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Const(1).toSuccess(), converter(AstReal(1, null)))
        assertEquals(Int32Const(2).toSuccess(), converter(AstReal(2, Builtin.LIT_INT32)))
        assertEquals(Int64Const(3).toSuccess(), converter(AstReal(3, Builtin.LIT_INT64)))
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
        
        assertEquals(expected.toFailure(), converter(AstReal(1, "unknown")))
    }
}

class TestConverterSubtract
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterSubtract(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.MINUS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("lhs", Builtin.INT32), thirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Sub(1.thir, 2.thir).toSuccess(), converter(AstSubtract(1.ast, 2.ast)))
        assertEquals(Int64Sub(1L.thir, 2L.thir).toSuccess(), converter(AstSubtract(1L.ast, 2L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.thir, 2L.thir)).toSuccess(), converter(AstSubtract(1.ast, 2L.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.MINUS.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(AstSubtract(true.ast, false.ast)))
    }
}

class TestConverterText
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterText(symbols)
    
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
        
        assertEquals(expected.toFailure(), converter(AstText("", "unknown")))
    }
}

class TestConverterUnaryMinus
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterUnaryMinus(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.MINUS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("rhs", Builtin.BOOL)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Neg(1.thir).toSuccess(), converter(AstMinus(1.ast)))
        assertEquals(Int64Neg(1L.thir).toSuccess(), converter(AstMinus(1L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(true.thir)).toSuccess(), converter(AstMinus(true.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.MINUS.symbol, listOf(Builtin.VOID))
        
        assertEquals(expected.toFailure(), converter(AstMinus("".ast)))
    }
}

class TestConverterUnaryPlus
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterUnaryPlus(symbols)
    
    private val function = thirFunOf(
        name = SymbolType.PLUS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(thirParOf("rhs", Builtin.BOOL)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(1.thir.toSuccess(), converter(AstPlus(1.ast)))
        assertEquals(1L.thir.toSuccess(), converter(AstPlus(1L.ast)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(true.thir)).toSuccess(), converter(AstPlus(true.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.PLUS.symbol, listOf(Builtin.VOID))
        
        assertEquals(expected.toFailure(), converter(AstPlus("".ast)))
    }
}

class TestConverterXor
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterXor(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolXor(true.thir, false.thir).toSuccess(), converter(AstXor(true.ast, false.ast)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.XOR.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(AstXor(false.ast, 0.ast)))
    }
}
