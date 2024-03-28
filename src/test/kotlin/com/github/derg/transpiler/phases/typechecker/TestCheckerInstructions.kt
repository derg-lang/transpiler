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
        fun `Given valid value type, when checking, then correct outcome`()
        {
            assertSuccess(Unit, check(true.thirBranch()))
        }
        
        @Test
        fun `Given invalid value type, when checking, then correct error`()
        {
            val expected = TypeError.BranchPredicateNotBool(0.thir)
            
            assertFailure(expected, check(0.thirBranch()))
        }
        
        @Test
        fun `Given valid error type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = thirTypeData(Builtin.BOOL.id), error = null).thirCall()
            
            assertSuccess(Unit, check(input.thirBranch()))
        }
        
        @Test
        fun `Given invalid error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData(Builtin.BOOL.id), error = thirTypeData()).thirCall()
            val expected = TypeError.BranchPredicateHasError(input)
            
            assertFailure(expected, check(input.thirBranch()))
        }
    }
}
