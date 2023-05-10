package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Access
import com.github.derg.transpiler.source.ast.Constant
import com.github.derg.transpiler.source.ast.Operator
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.util.toFailure
import com.github.derg.transpiler.util.toSuccess
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class TestConverterExpressions
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterExpressions(symbols)
    
    @Test
    fun `Given invoke expression, when resolving, then correctly resolved`()
    {
        val function = hirFunOf("foo", valueType = Builtin.INT32).also { symbols.register(it) }
        
        assertEquals(
            Int32Call(function, emptyList()).toSuccess(),
            converter.convert(Access.Function("foo", emptyList())),
        )
        
        assertEquals(
            ResolveError.UnknownCallable("bar").toFailure(),
            converter.convert(Access.Function("bar", emptyList())),
        )
    }
    
    @Test
    fun `Given access expression, when resolving, then correctly resolved`()
    {
        val boolVar = hirVarOf("var_bool", Builtin.BOOL).also { symbols.register(it) }
        val int32Var = hirVarOf("var_int32", Builtin.INT32).also { symbols.register(it) }
        val int64Var = hirVarOf("var_int64", Builtin.INT64).also { symbols.register(it) }
        
        assertEquals(BoolRead(boolVar).toSuccess(), converter.convert(Access.Variable(boolVar.name)))
        assertEquals(Int32Read(int32Var).toSuccess(), converter.convert(Access.Variable(int32Var.name)))
        assertEquals(Int64Read(int64Var).toSuccess(), converter.convert(Access.Variable(int64Var.name)))
        
        assertEquals(ResolveError.Unknown.toFailure(), converter.convert(Access.Variable("foo")))
    }
    
    @Test
    fun `Given const bool expression, when resolving, then correctly resolved`()
    {
        assertEquals(BoolConst(true).toSuccess(), converter.convert(Constant.Bool(true)))
        assertEquals(BoolConst(false).toSuccess(), converter.convert(Constant.Bool(false)))
    }
    
    @Test
    fun `Given const real expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Const(1).toSuccess(), converter.convert(Constant.Real(1, null))) // Default literal
        assertEquals(Int32Const(2).toSuccess(), converter.convert(Constant.Real(2, Builtin.LIT_INT32)))
        assertEquals(Int64Const(3L).toSuccess(), converter.convert(Constant.Real(3, Builtin.LIT_INT64)))
        
        assertEquals(ResolveError.UnknownLiteral("foo").toFailure(), converter.convert(Constant.Real(0, "foo")))
    }
    
    @Test
    fun `Given add expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Add(1.v, 2.v).toSuccess(), converter.convert(Operator.Add(1.e, 2.e)))
        assertEquals(Int64Add(1L.v, 2L.v).toSuccess(), converter.convert(Operator.Add(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Add(false.e, 0.e)))
    }
    
    @Test
    fun `Given divide expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Div(1.v, 2.v).toSuccess(), converter.convert(Operator.Divide(1.e, 2.e)))
        assertEquals(Int64Div(1L.v, 2L.v).toSuccess(), converter.convert(Operator.Divide(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Divide(false.e, 0.e)))
    }
    
    @Test
    fun `Given equal expression, when resolving, then correctly resolved`()
    {
        assertEquals(BoolEq(true.v, false.v).toSuccess(), converter.convert(Operator.Equal(true.e, false.e)))
        assertEquals(Int32Eq(1.v, 2.v).toSuccess(), converter.convert(Operator.Equal(1.e, 2.e)))
        assertEquals(Int64Eq(1L.v, 2L.v).toSuccess(), converter.convert(Operator.Equal(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Equal(false.e, 0.e)))
    }
    
    @Test
    fun `Given greater expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Gt(1.v, 2.v).toSuccess(), converter.convert(Operator.Greater(1.e, 2.e)))
        assertEquals(Int64Gt(1L.v, 2L.v).toSuccess(), converter.convert(Operator.Greater(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Greater(false.e, 0.e)))
    }
    
    @Test
    fun `Given greater equal expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Ge(1.v, 2.v).toSuccess(), converter.convert(Operator.GreaterEqual(1.e, 2.e)))
        assertEquals(Int64Ge(1L.v, 2L.v).toSuccess(), converter.convert(Operator.GreaterEqual(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.GreaterEqual(false.e, 0.e)))
    }
    
    @Test
    fun `Given lesser expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Lt(1.v, 2.v).toSuccess(), converter.convert(Operator.Less(1.e, 2.e)))
        assertEquals(Int64Lt(1L.v, 2L.v).toSuccess(), converter.convert(Operator.Less(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Less(false.e, 0.e)))
    }
    
    @Test
    fun `Given lesser equal expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Le(1.v, 2.v).toSuccess(), converter.convert(Operator.LessEqual(1.e, 2.e)))
        assertEquals(Int64Le(1L.v, 2L.v).toSuccess(), converter.convert(Operator.LessEqual(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.LessEqual(false.e, 0.e)))
    }
    
    @Test
    fun `Given modulo expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Mod(1.v, 2.v).toSuccess(), converter.convert(Operator.Modulo(1.e, 2.e)))
        assertEquals(Int64Mod(1L.v, 2L.v).toSuccess(), converter.convert(Operator.Modulo(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Modulo(false.e, 0.e)))
    }
    
    @Test
    fun `Given multiply expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Mul(1.v, 2.v).toSuccess(), converter.convert(Operator.Multiply(1.e, 2.e)))
        assertEquals(Int64Mul(1L.v, 2L.v).toSuccess(), converter.convert(Operator.Multiply(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Multiply(false.e, 0.e)))
    }
    
    @Test
    fun `Given not equal expression, when resolving, then correctly resolved`()
    {
        assertEquals(BoolNe(true.v, false.v).toSuccess(), converter.convert(Operator.NotEqual(true.e, false.e)))
        assertEquals(Int32Ne(1.v, 2.v).toSuccess(), converter.convert(Operator.NotEqual(1.e, 2.e)))
        assertEquals(Int64Ne(1L.v, 2L.v).toSuccess(), converter.convert(Operator.NotEqual(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.NotEqual(false.e, 0.e)))
    }
    
    @Test
    fun `Given subtract expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Sub(1.v, 2.v).toSuccess(), converter.convert(Operator.Subtract(1.e, 2.e)))
        assertEquals(Int64Sub(1L.v, 2L.v).toSuccess(), converter.convert(Operator.Subtract(1L.e, 2L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Subtract(false.e, 0.e)))
    }
    
    @Test
    fun `Given unary minus expression, when resolving, then correctly resolved`()
    {
        assertEquals(Int32Neg(1.v).toSuccess(), converter.convert(Operator.Minus(1.e)))
        assertEquals(Int64Neg(1L.v).toSuccess(), converter.convert(Operator.Minus(1L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Minus(false.e)))
    }
    
    @Test
    fun `Given unary plus expression, when resolving, then correctly resolved`()
    {
        assertEquals(1.v.toSuccess(), converter.convert(Operator.Plus(1.e)))
        assertEquals(1L.v.toSuccess(), converter.convert(Operator.Plus(1L.e)))
        
        assertEquals(ResolveError.Unsupported.toFailure(), converter.convert(Operator.Plus(false.e)))
    }
}
