package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.util.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestConverterAssign
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterAssign(symbols)
    
    private val variable = thirVarOf("foo", type = Builtin.INT32).also { symbols.register(it) }
    
    @Test
    fun `Given valid value, when resolving, then correct outcome`()
    {
        assertEquals(Assign(variable, 1.thir).toSuccess(), converter(AstAssign(variable.name, 1.ast)))
    }
    
    @Test
    fun `Given invalid value, when resolving, then correct error`()
    {
        val expected = ResolveError.MismatchedVariableType(expected = variable.type, actual = Builtin.BOOL)
        
        assertEquals(expected.toFailure(), converter(AstAssign(variable.name, true.ast)))
    }
    
    @Test
    fun `Given unknown variable, when resolving, then correct error`()
    {
        val expected = ResolveError.UnknownVariable("unknown")
        
        assertEquals(expected.toFailure(), converter(AstAssign("unknown", 1.ast)))
    }
}

class TestConverterBranch
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterBranch(symbols)
    
    /**
     * Small helper for generating the actual input for resolving a branch statement.
     */
    private fun inputOf(
        predicate: AstExpression,
        success: List<AstStatement> = emptyList(),
        failure: List<AstStatement> = emptyList(),
    ) = AstBranch(predicate, success = success, failure = failure)
    
    /**
     * Small helper for generating the expected outcome from resolving a branch statement.
     */
    private fun expectedOf(
        predicate: ValueBool,
        success: List<Instruction> = emptyList(),
        failure: List<Instruction> = emptyList(),
    ) = Condition(predicate, success = Scope(success, symbols), failure = Scope(failure, symbols))
    
    @Test
    fun `Given valid predicate, when resolving, then correct outcome`()
    {
        assertEquals(expectedOf(BoolConst(true)).toSuccess(), converter(inputOf(true.ast)))
    }
    
    @Test
    fun `Given invalid predicate, when resolving, then correct error`()
    {
        val expected = ResolveError.InvalidPredicateType(Builtin.INT32)
        
        assertEquals(expected.toFailure(), converter(inputOf(1.ast)))
    }
    
    @Test
    fun `Given success, when resolving, then correct outcome`()
    {
        val expected = expectedOf(BoolConst(true), success = listOf(Exit))
        
        assertEquals(expected.toSuccess(), converter(inputOf(true.ast, success = listOf(AstReturnValue(null)))))
    }
    
    @Test
    fun `Given failure, when resolving, then correct outcome`()
    {
        val expected = expectedOf(BoolConst(true), failure = listOf(Exit))
        
        assertEquals(expected.toSuccess(), converter(inputOf(true.ast, failure = listOf(AstReturnValue(null)))))
    }
    
    @Test
    fun `When resolving, then branches have independent scopes`()
    {
        val input = inputOf(true.ast, success = listOf(astVarOf("a", 1.ast)), failure = listOf(astVarOf("b", 1.ast)))
        val actual = converter(input).valueOrDie() as Condition
        
        assertNotEquals(emptyList<Symbol>(), actual.success.symbols.find("a"))
        assertEquals(emptyList<Symbol>(), actual.success.symbols.find("b"))
        assertEquals(emptyList<Symbol>(), actual.failure.symbols.find("a"))
        assertNotEquals(emptyList<Symbol>(), actual.failure.symbols.find("b"))
    }
}

class TestConverterRaise
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterRaise(symbols)
    
    @Test
    fun `Given valid value, when resolving, then correct outcome`()
    {
        assertEquals(Raise(1.thir).toSuccess(), converter(AstReturnError(1.ast)))
    }
}

class TestConverterReturn
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterReturn(symbols)
    
    @Test
    fun `Given no value, when resolving, then correct outcome`()
    {
        assertEquals(Exit.toSuccess(), converter(AstReturnValue(null)))
    }
    
    @Test
    fun `Given valid value, when resolving, then correct outcome`()
    {
        assertEquals(Return(1.thir).toSuccess(), converter(AstReturnValue(1.ast)))
    }
}
