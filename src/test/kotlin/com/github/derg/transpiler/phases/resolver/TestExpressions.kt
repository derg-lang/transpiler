package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.parser.toArg
import com.github.derg.transpiler.phases.parser.toExp
import com.github.derg.transpiler.phases.resolver.ResolveError.MismatchedFunctionTypes
import com.github.derg.transpiler.source.ast.Access
import com.github.derg.transpiler.source.ast.Constant
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
        val expected = MismatchedFunctionTypes(SymbolType.AND.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Add(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.PLUS.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.Add(true.e, false.e)))
    }
}

class TestConverterBool
{
    private val converter = ConverterBool
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolConst(true).toSuccess(), converter(Constant.Bool(true)))
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Divide(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.DIVIDE.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Equal(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.EQUAL.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Greater(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.GREATER.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.GreaterEqual(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.GREATER_EQUAL.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.GreaterEqual(true.e, false.e)))
    }
}

class TestConverterInvoke
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterInvoke(symbols)
    
    private val bool = hirFunOf(name = "bool", valueType = Builtin.BOOL).also { symbols.register(it) }
    private val int32 = hirFunOf(name = "int32", valueType = Builtin.INT32).also { symbols.register(it) }
    private val int64 = hirFunOf(name = "int64", valueType = Builtin.INT64).also { symbols.register(it) }
    
    private val params = hirFunOf(
        name = "params",
        valueType = Builtin.BOOL,
        params = listOf(hirParOf("foo", Builtin.INT32), hirParOf("bar", Builtin.INT64)),
    ).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(bool, emptyList()).toSuccess(), converter(Access.Function(bool.name, emptyList())))
        assertEquals(Int32Call(int32, emptyList()).toSuccess(), converter(Access.Function(int32.name, emptyList())))
        assertEquals(Int64Call(int64, emptyList()).toSuccess(), converter(Access.Function(int64.name, emptyList())))
    }
    
    @Test
    fun `Given unknown function, when resolving, then correct error`()
    {
        val expected = ResolveError.UnknownFunction("unknown")
        
        assertEquals(expected.toFailure(), converter(Access.Function("unknown", emptyList())))
    }
    
    @Test
    fun `Given valid parameters, when resolving, then correct outcome`()
    {
        val input = Access.Function(params.name, listOf(1.toArg(), 2.toExp(Builtin.LIT_INT64).toArg()))
        val expected = BoolCall(params, listOf(1.v, 2L.v))
        
        assertEquals(expected.toSuccess(), converter(input))
    }
    
    @Test
    fun `Given invalid parameters, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(params.name, listOf(Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Access.Function(params.name, listOf(true.toArg()))))
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Less(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.LESS.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.LessEqual(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.LESS_EQUAL.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Modulo(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.MODULO.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Multiply(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.MULTIPLY.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
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
        val expected = MismatchedFunctionTypes(SymbolType.NOT.symbol, listOf(Builtin.INT32))
        
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.NotEqual(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.NOT_EQUAL.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
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
        val expected = MismatchedFunctionTypes(SymbolType.OR.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.Or(false.e, 0.e)))
    }
}

class TestConverterRead
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterRead(symbols)
    
    private val bool = hirVarOf(name = "bool", type = Builtin.BOOL).also { symbols.register(it) }
    private val int32 = hirVarOf(name = "int32", type = Builtin.INT32).also { symbols.register(it) }
    private val int64 = hirVarOf(name = "int64", type = Builtin.INT64).also { symbols.register(it) }
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(BoolRead(bool).toSuccess(), converter(Access.Variable(bool.name)))
        assertEquals(Int32Read(int32).toSuccess(), converter(Access.Variable(int32.name)))
        assertEquals(Int64Read(int64).toSuccess(), converter(Access.Variable(int64.name)))
    }
    
    @Test
    fun `Given unknown variable, when resolving, then correct error`()
    {
        val expected = ResolveError.UnknownVariable("unknown")
        
        assertEquals(expected.toFailure(), converter(Access.Variable("unknown")))
    }
}

class TestConverterReal
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterReal(symbols)
    
    @Test
    fun `Given builtin types, when resolving, then correct outcome`()
    {
        assertEquals(Int32Const(1).toSuccess(), converter(Constant.Real(1, null)))
        assertEquals(Int32Const(2).toSuccess(), converter(Constant.Real(2, Builtin.LIT_INT32)))
        assertEquals(Int64Const(3).toSuccess(), converter(Constant.Real(3, Builtin.LIT_INT64)))
    }
    
    @Test
    fun `Given known overload, when resolving, then correct outcome`()
    {
        // TODO: Implement me
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = ResolveError.UnknownLiteral("unknown")
        
        assertEquals(expected.toFailure(), converter(Constant.Real(1, "unknown")))
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(1.v, 2L.v)).toSuccess(), converter(Operator.Subtract(1.e, 2L.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.MINUS.symbol, listOf(Builtin.BOOL, Builtin.BOOL))
        
        assertEquals(expected.toFailure(), converter(Operator.Subtract(true.e, false.e)))
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
        val expected = ResolveError.UnknownLiteral("unknown")
        
        assertEquals(expected.toFailure(), converter(Constant.Text("", "unknown")))
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(true.v)).toSuccess(), converter(Operator.Minus(true.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.MINUS.symbol, listOf(Builtin.VOID))
        
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
    fun `Given known overload, when resolving, then correct outcome`()
    {
        assertEquals(BoolCall(function, listOf(true.v)).toSuccess(), converter(Operator.Plus(true.e)))
    }
    
    @Test
    fun `Given unknown overload, when resolving, then correct error`()
    {
        val expected = MismatchedFunctionTypes(SymbolType.PLUS.symbol, listOf(Builtin.VOID))
        
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
        val expected = MismatchedFunctionTypes(SymbolType.XOR.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.Xor(false.e, 0.e)))
    }
}