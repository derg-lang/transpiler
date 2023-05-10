package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Access
import com.github.derg.transpiler.source.ast.Constant
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
}
