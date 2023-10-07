package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*

class TestConverterStatements
{
    private val symbols = ThirSymbolTable(Builtin.SYMBOLS)
    private val converter = ConverterStatements(symbols)
    
    @Nested
    inner class Assign
    {
        private val variable = thirVarOf(Builtin.INT32).also(symbols::register)
        
        @Test
        fun `Given valid type, when resolving, then correct outcome`()
        {
            assertSuccess(variable thirAssign 1, converter(variable.name astAssign 1))
        }
        
        @Test
        fun `Given invalid type, when resolving, then correct error`()
        {
            val expected = ResolveError.MismatchedVariableType(variable.type, Builtin.BOOL.id)
            
            assertFailure(expected, converter(variable.name astAssign true))
        }
        
        @Test
        fun `Given unknown variable, when resolving, then correct error`()
        {
            val expected = ResolveError.UnknownVariable("unknown")
            
            assertFailure(expected, converter("unknown" astAssign 1))
        }
    }
    
    // TODO: Refactor a lot of the stuff here at some point, this needs to be cleaned up
    @Nested
    inner class Branch
    {
        /**
         * Small helper for generating the actual input for resolving a branch statement.
         */
        private fun inputOf(
            predicate: AstExpression = true.ast,
            success: List<AstStatement> = emptyList(),
            failure: List<AstStatement> = emptyList(),
        ) = AstBranch(predicate, success = success, failure = failure)
        
        /**
         * Small helper for generating the expected outcome from resolving a branch statement.
         */
        private fun expectedOf(
            predicate: ThirValue = true.thir,
            success: List<ThirInstruction> = emptyList(),
            failure: List<ThirInstruction> = emptyList(),
        ) = ThirBranch(predicate, success = ThirScope(symbols), failure = ThirScope(symbols))
            .also { it.success.instructions.addAll(success) }
            .also { it.failure.instructions.addAll(failure) }
        
        @Test
        fun `Given valid predicate, when resolving, then correct outcome`()
        {
            assertSuccess(expectedOf(true.thir), converter(inputOf(true.ast)))
        }
        
        @Test
        fun `Given invalid value type, when resolving, then correct error`()
        {
            val expected = ResolveError.MismatchedPredicateType(Builtin.BOOL.id, Builtin.INT32.id)
            
            assertFailure(expected, converter(inputOf(1.ast)))
        }
        
        // TODO: Requires some overhauling to the AST error nodes - no way to catch errors as of yet
        @Disabled
        @Test
        fun `Given invalid error type, when resolving, then correct error`()
        {
            val expected = ResolveError.MismatchedPredicateType(Builtin.VOID.id, Builtin.INT32.id)
            
            assertFailure(expected, converter(inputOf(1.astRaise(1)))) // TODO: 1.astRaise is enough
        }
        
        @Test
        fun `Given success, when resolving, then correct outcome`()
        {
            val expected = expectedOf(success = listOf(ThirReturn))
            
            assertSuccess(expected, converter(inputOf(success = listOf(AstReturn))))
        }
        
        @Test
        fun `Given failure, when resolving, then correct outcome`()
        {
            val expected = expectedOf(failure = listOf(ThirReturn))
            
            assertSuccess(expected, converter(inputOf(failure = listOf(AstReturn))))
        }
        
        @Test
        fun `When resolving, then branches have independent scopes`()
        {
            val input = inputOf(
                predicate = true.ast,
                success = listOf(astVarOf("a", 1.ast)),
                failure = listOf(astVarOf("b", 1.ast)),
            )
            val actual = converter(input).valueOrDie() as ThirBranch
            
            assertNotEquals(emptyList<ThirSymbol>(), actual.success.symbols["a"])
            assertEquals(emptyList<ThirSymbol>(), actual.success.symbols["b"])
            assertEquals(emptyList<ThirSymbol>(), actual.failure.symbols["a"])
            assertNotEquals(emptyList<ThirSymbol>(), actual.failure.symbols["b"])
        }
    }
    
    @Nested
    inner class Evaluate
    {
        @Test
        fun `Given valid statement, when resolving, then correct outcome`()
        {
            val function = thirFunOf(valType = Builtin.VOID, errType = Builtin.VOID).also(symbols::register)
            
            assertSuccess(function.thirCall().thirEval, converter(function.name.astCall().astEval))
        }
        
        @Test
        fun `Given non-void value statement, when resolving, then correct outcome`()
        {
            val function = thirFunOf(valType = Builtin.BOOL).also(symbols::register)
            val expected = ResolveError.MismatchedEvaluationType(Builtin.VOID.id, Builtin.BOOL.id)
            
            assertFailure(expected, converter(function.name.astCall().astEval))
        }
        
        @Test
        fun `Given non-void error statement, when resolving, then correct outcome`()
        {
            val function = thirFunOf(errType = Builtin.BOOL).also(symbols::register)
            val expected = ResolveError.MismatchedEvaluationType(Builtin.VOID.id, Builtin.BOOL.id)
            
            assertFailure(expected, converter(function.name.astCall().astEval))
        }
    }
    
    @Nested
    inner class Return
    {
        @Test
        fun `Given void, when resolving, then correct outcome`()
        {
            assertSuccess(ThirReturn, converter(AstReturn))
        }
        
        @Test
        fun `Given value, when resolving, then correct outcome`()
        {
            assertSuccess(1.thirReturnValue, converter(1.astReturnValue))
        }
        
        @Test
        fun `Given error, when resolving, then correct outcome`()
        {
            assertSuccess(1.thirReturnError, converter(1.astReturnError))
        }
    }
}
