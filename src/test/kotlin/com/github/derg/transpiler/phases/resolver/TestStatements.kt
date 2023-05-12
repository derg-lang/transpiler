package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.parser.varOf
import com.github.derg.transpiler.source.ast.Assignment
import com.github.derg.transpiler.source.ast.Control
import com.github.derg.transpiler.source.ast.Expression
import com.github.derg.transpiler.source.ast.Statement
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.util.toFailure
import com.github.derg.transpiler.util.toSuccess
import com.github.derg.transpiler.util.valueOrDie
import org.junit.jupiter.api.Assertions.assertNotEquals
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class TestConverterAssign
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterAssign(symbols)
    
    private val variable = hirVarOf("foo", type = Builtin.INT32).also { symbols.register(it) }
    
    @Test
    fun `Given valid value, when resolving, then correct outcome`()
    {
        assertEquals(Assign(variable, 1.v).toSuccess(), converter(Assignment.Assign(variable.name, 1.e)))
    }
    
    @Test
    fun `Given invalid value, when resolving, then correct error`()
    {
        val expected = ResolveError.MismatchedVariableType(expected = variable.type, actual = Builtin.BOOL)
        
        assertEquals(expected.toFailure(), converter(Assignment.Assign(variable.name, true.e)))
    }
    
    @Test
    fun `Given unknown variable, when resolving, then correct error`()
    {
        val expected = ResolveError.UnknownVariable("unknown")
        
        assertEquals(expected.toFailure(), converter(Assignment.Assign("unknown", 1.e)))
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
        predicate: Expression,
        success: List<Statement> = emptyList(),
        failure: List<Statement> = emptyList(),
    ) = Control.Branch(predicate, success = success, failure = failure)
    
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
        assertEquals(expectedOf(BoolConst(true)).toSuccess(), converter(inputOf(true.e)))
    }
    
    @Test
    fun `Given invalid predicate, when resolving, then correct error`()
    {
        val expected = ResolveError.InvalidPredicateType(Builtin.INT32)
        
        assertEquals(expected.toFailure(), converter(inputOf(1.e)))
    }
    
    @Test
    fun `Given success, when resolving, then correct outcome`()
    {
        val expected = expectedOf(BoolConst(true), success = listOf(Exit))
        
        assertEquals(expected.toSuccess(), converter(inputOf(true.e, success = listOf(Control.Return(null)))))
    }
    
    @Test
    fun `Given failure, when resolving, then correct outcome`()
    {
        val expected = expectedOf(BoolConst(true), failure = listOf(Exit))
        
        assertEquals(expected.toSuccess(), converter(inputOf(true.e, failure = listOf(Control.Return(null)))))
    }
    
    @Test
    fun `When resolving, then branches have independent scopes`()
    {
        val input = inputOf(true.e, success = listOf(varOf("foo", 1.e)), failure = listOf(varOf("bar", 1.e)))
        val actual = converter(input).valueOrDie() as Condition
        
        assertNotEquals(emptyList<Symbol>(), actual.success.symbols.find("foo"))
        assertEquals(emptyList(), actual.success.symbols.find("bar"))
        assertEquals(emptyList(), actual.failure.symbols.find("foo"))
        assertNotEquals(emptyList<Symbol>(), actual.failure.symbols.find("bar"))
    }
}

class TestConverterRaise
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterRaise(symbols)
    
    @Test
    fun `Given valid value, when resolving, then correct outcome`()
    {
        assertEquals(Raise(1.v).toSuccess(), converter(Control.Raise(1.e)))
    }
}

class TestConverterReturn
{
    private val symbols = SymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterReturn(symbols)
    
    @Test
    fun `Given no value, when resolving, then correct outcome`()
    {
        assertEquals(Exit.toSuccess(), converter(Control.Return(null)))
    }
    
    @Test
    fun `Given valid value, when resolving, then correct outcome`()
    {
        assertEquals(Return(1.v).toSuccess(), converter(Control.Return(1.e)))
    }
}
