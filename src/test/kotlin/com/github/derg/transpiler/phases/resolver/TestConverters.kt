package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.resolver.ResolveError.MismatchedCallableParams
import com.github.derg.transpiler.source.ast.Operator
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.lexeme.SymbolType
import com.github.derg.transpiler.util.toFailure
import com.github.derg.transpiler.util.toSuccess
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class TestConverterAnd
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterAnd(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolAnd(true.v, false.v).toSuccess(), converter(Operator.And(true.e, false.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.AND.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.And(false.e, 0.e)))
    }
}

class TestConverterAdd
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterAdd(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.PLUS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Add(1.v, 2.v).toSuccess(), converter(Operator.Add(1.e, 2.e)))
        assertEquals(Int64Add(1L.v, 2L.v).toSuccess(), converter(Operator.Add(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Add(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.PLUS.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.Add(true.e, false.e)))
    }
}

class TestConverterDivide
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterDivide(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.DIVIDE.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Div(1.v, 2.v).toSuccess(), converter(Operator.Divide(1.e, 2.e)))
        assertEquals(Int64Div(1L.v, 2L.v).toSuccess(), converter(Operator.Divide(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Divide(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.DIVIDE.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.Divide(true.e, false.e)))
    }
}

class TestConverterEqual
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterEqual(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.EQUAL.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolEq(true.v, false.v).toSuccess(), converter(Operator.Equal(true.e, false.e)))
        assertEquals(Int32Eq(1.v, 2.v).toSuccess(), converter(Operator.Equal(1.e, 2.e)))
        assertEquals(Int64Eq(1L.v, 2L.v).toSuccess(), converter(Operator.Equal(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Equal(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.EQUAL.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.Equal(true.e, 1.e)))
    }
}

class TestConverterGreater
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterGreater(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.GREATER.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Gt(1.v, 2.v).toSuccess(), converter(Operator.Greater(1.e, 2.e)))
        assertEquals(Int64Gt(1L.v, 2L.v).toSuccess(), converter(Operator.Greater(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Greater(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.GREATER.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.Greater(true.e, false.e)))
    }
}

class TestConverterGreaterEqual
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterGreaterEqual(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.GREATER_EQUAL.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Ge(1.v, 2.v).toSuccess(), converter(Operator.GreaterEqual(1.e, 2.e)))
        assertEquals(Int64Ge(1L.v, 2L.v).toSuccess(), converter(Operator.GreaterEqual(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.GreaterEqual(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.GREATER_EQUAL.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.GreaterEqual(true.e, false.e)))
    }
}

class TestConverterLess
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterLess(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.LESS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Lt(1.v, 2.v).toSuccess(), converter(Operator.Less(1.e, 2.e)))
        assertEquals(Int64Lt(1L.v, 2L.v).toSuccess(), converter(Operator.Less(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Less(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.LESS.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.Less(true.e, false.e)))
    }
}

class TestConverterLessEqual
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterLessEqual(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.LESS_EQUAL.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Le(1.v, 2.v).toSuccess(), converter(Operator.LessEqual(1.e, 2.e)))
        assertEquals(Int64Le(1L.v, 2L.v).toSuccess(), converter(Operator.LessEqual(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.LessEqual(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.LESS_EQUAL.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.LessEqual(true.e, false.e)))
    }
}

class TestConverterModulo
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterModulo(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.MODULO.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Mod(1.v, 2.v).toSuccess(), converter(Operator.Modulo(1.e, 2.e)))
        assertEquals(Int64Mod(1L.v, 2L.v).toSuccess(), converter(Operator.Modulo(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Modulo(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.MODULO.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.Modulo(true.e, false.e)))
    }
}

class TestConverterMultiply
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterMultiply(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.MULTIPLY.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Mul(1.v, 2.v).toSuccess(), converter(Operator.Multiply(1.e, 2.e)))
        assertEquals(Int64Mul(1L.v, 2L.v).toSuccess(), converter(Operator.Multiply(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Multiply(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.MULTIPLY.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.Multiply(true.e, false.e)))
    }
}

class TestConverterNot
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterNot(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolNot(true.v).toSuccess(), converter(Operator.Not(true.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.NOT.symbol, listOf(Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.Not(0.e)))
    }
}

class TestConverterNotEqual
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterNotEqual(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.NOT_EQUAL.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolNe(true.v, false.v).toSuccess(), converter(Operator.NotEqual(true.e, false.e)))
        assertEquals(Int32Ne(1.v, 2.v).toSuccess(), converter(Operator.NotEqual(1.e, 2.e)))
        assertEquals(Int64Ne(1L.v, 2L.v).toSuccess(), converter(Operator.NotEqual(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.NotEqual(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.NOT_EQUAL.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.NotEqual(true.e, 1.e)))
    }
}

class TestConverterOr
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterOr(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolOr(true.v, false.v).toSuccess(), converter(Operator.Or(true.e, false.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.OR.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.Or(false.e, 0.e)))
    }
}

class TestConverterSubtract
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterSubtract(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.MINUS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("lhs", Builtin.INT32), hirParOf("rhs", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Sub(1.v, 2.v).toSuccess(), converter(Operator.Subtract(1.e, 2.e)))
        assertEquals(Int64Sub(1L.v, 2L.v).toSuccess(), converter(Operator.Subtract(1L.e, 2L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Subtract(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.MINUS.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.Subtract(true.e, false.e)))
    }
}

class TestConverterUnaryMinus
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterUnaryMinus(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.MINUS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("rhs", Builtin.BOOL)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Neg(1.v).toSuccess(), converter(Operator.Minus(1.e)))
        assertEquals(Int64Neg(1L.v).toSuccess(), converter(Operator.Minus(1L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(true.v)).toSuccess(), converter(Operator.Minus(true.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.MINUS.symbol, listOf(Builtin.VOID))
        
        assertEquals(expected.toFailure(), converter(Operator.Minus("".e)))
    }
}

class TestConverterUnaryPlus
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterUnaryPlus(symbols)
    
    private val function = hirFunOf(
        name = SymbolType.PLUS.symbol,
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("rhs", Builtin.BOOL)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(1.v.toSuccess(), converter(Operator.Plus(1.e)))
        assertEquals(1L.v.toSuccess(), converter(Operator.Plus(1L.e)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct error`()
    {
        assertEquals(BoolCall(function, listOf(true.v)).toSuccess(), converter(Operator.Plus(true.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.PLUS.symbol, listOf(Builtin.VOID))
        
        assertEquals(expected.toFailure(), converter(Operator.Plus("".e)))
    }
}

class TestConverterXor
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterXor(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolXor(true.v, false.v).toSuccess(), converter(Operator.Xor(true.e, false.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedCallableParams(SymbolType.XOR.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.Xor(false.e, 0.e)))
    }
}
