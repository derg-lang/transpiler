package com.github.derg.transpiler.phases.resolver

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
        val expected = ResolveError.MismatchedCallableParams(SymbolType.AND.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.And(false.e, 0.e)))
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
        val expected = ResolveError.MismatchedCallableParams(SymbolType.NOT.symbol, listOf(Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.Not(0.e)))
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
        val expected = ResolveError.MismatchedCallableParams(SymbolType.OR.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.Or(false.e, 0.e)))
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
        val expected = ResolveError.MismatchedCallableParams(SymbolType.XOR.symbol, listOf(Builtin.BOOL, Builtin.INT32))
        
        assertEquals(expected.toFailure(), converter(Operator.Xor(false.e, 0.e)))
    }
}
