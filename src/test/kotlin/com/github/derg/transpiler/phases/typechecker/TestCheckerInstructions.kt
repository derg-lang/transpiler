package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

class TestCheckerInstructions
{
    @Nested
    inner class Branch
    {
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = thirTypeData(Builtin.BOOL.id), error = null).thirCall()
            
            assertSuccess(Unit, check(input.thirBranch()))
        }
        
        @Test
        fun `Given invalid value type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData(), error = null).thirCall()
            val expected = TypeError.BranchPredicateNotBool(input)
            
            assertFailure(expected, check(input.thirBranch()))
        }
        
        @Test
        fun `Given invalid error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData(Builtin.BOOL.id), error = thirTypeData()).thirCall()
            val expected = TypeError.BranchPredicateHasError(input)
            
            assertFailure(expected, check(input.thirBranch()))
        }
    }
    
    @Nested
    inner class Evaluate
    {
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = null, error = null).thirCall()
            
            assertSuccess(Unit, check(input.thirEval))
        }
        
        @Test
        fun `Given invalid value type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData(), error = null).thirCall()
            val expected = TypeError.EvaluateHasValue(input)
            
            assertFailure(expected, check(input.thirEval))
        }
        
        @Test
        fun `Given invalid error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null, error = thirTypeData()).thirCall()
            val expected = TypeError.EvaluateHasError(input)
            
            assertFailure(expected, check(input.thirEval))
        }
    }
}
