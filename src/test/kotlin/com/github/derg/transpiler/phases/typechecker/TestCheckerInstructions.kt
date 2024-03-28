package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

class TestCheckerInstructions
{
    private val bool = thirTypeData(Builtin.BOOL.id)
    private val int32 = thirTypeData(Builtin.INT32.id)
    
    @Nested
    inner class Branch
    {
        private val checker = CheckerInstruction(value = null, error = null)
        
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = bool, error = null).thirCall()
            
            assertSuccess(Unit, checker.check(input.thirBranch()))
        }
        
        @Test
        fun `Given invalid value type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData(), error = null).thirCall()
            val expected = TypeError.BranchWrongValue(input)
            
            assertFailure(expected, checker.check(input.thirBranch()))
        }
        
        @Test
        fun `Given invalid error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = bool, error = thirTypeData()).thirCall()
            val expected = TypeError.BranchContainsError(input)
            
            assertFailure(expected, checker.check(input.thirBranch()))
        }
    }
    
    @Nested
    inner class Evaluate
    {
        private val checker = CheckerInstruction(value = null, error = null)
        
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = null, error = null).thirCall()
            
            assertSuccess(Unit, checker.check(input.thirEval))
        }
        
        @Test
        fun `Given invalid value type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData(), error = null).thirCall()
            val expected = TypeError.EvaluateContainsValue(input)
            
            assertFailure(expected, checker.check(input.thirEval))
        }
        
        @Test
        fun `Given invalid error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null, error = thirTypeData()).thirCall()
            val expected = TypeError.EvaluateContainsError(input)
            
            assertFailure(expected, checker.check(input.thirEval))
        }
    }
    
    @Nested
    inner class Return
    {
        private val checkerEmpty = CheckerInstruction(value = null, error = null)
        private val checkerValue = CheckerInstruction(value = bool, error = null)
        
        @Test
        fun `Given no types, when checking, then correct outcome`()
        {
            assertSuccess(Unit, checkerEmpty.check(ThirReturn))
        }
        
        @Test
        fun `Given value type, when checking, then correct error`()
        {
            assertFailure(TypeError.ReturnMissingExpression, checkerValue.check(ThirReturn))
        }
    }
    
    @Nested
    inner class ReturnValue
    {
        private val checkerEmpty = CheckerInstruction(value = null, error = null)
        private val checkerValue = CheckerInstruction(value = bool, error = null)
        
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = bool).thirCall()
            
            assertSuccess(Unit, checkerValue.check(input.thirReturnValue))
        }
        
        @Test
        fun `Given no type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null).thirCall()
            
            assertFailure(TypeError.ReturnMissingValue(input), checkerValue.check(input.thirReturnValue))
        }
        
        @Test
        fun `Given invalid type, when checking, then correct error`()
        {
            val input = thirFunOf(value = int32).thirCall()
            
            assertFailure(TypeError.ReturnWrongType(input), checkerValue.check(input.thirReturnValue))
        }
        
        @Test
        fun `Given error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = bool, error = bool).thirCall()
            
            assertFailure(TypeError.ReturnContainsError(input), checkerValue.check(input.thirReturnValue))
        }
        
        @Test
        fun `Given bad return, when checking, then correct error`()
        {
            val input = thirFunOf(value = int32).thirCall()
            
            assertFailure(TypeError.ReturnContainsValue(input), checkerEmpty.check(input.thirReturnValue))
        }
    }
    
    @Nested
    inner class ReturnError
    {
        private val checkerEmpty = CheckerInstruction(value = null, error = null)
        private val checkerError = CheckerInstruction(value = null, error = bool)
        
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = bool).thirCall()
            
            assertSuccess(Unit, checkerError.check(input.thirReturnError))
        }
        
        @Test
        fun `Given no type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null).thirCall()
            
            assertFailure(TypeError.ReturnMissingValue(input), checkerError.check(input.thirReturnError))
        }
        
        @Test
        fun `Given invalid type, when checking, then correct error`()
        {
            val input = thirFunOf(value = int32).thirCall()
            
            assertFailure(TypeError.ReturnWrongType(input), checkerError.check(input.thirReturnError))
        }
        
        @Test
        fun `Given error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = bool, error = bool).thirCall()
            
            assertFailure(TypeError.ReturnContainsError(input), checkerError.check(input.thirReturnError))
        }
        
        @Test
        fun `Given bad return, when checking, then correct error`()
        {
            val input = thirFunOf(value = int32).thirCall()
            
            assertFailure(TypeError.ReturnContainsValue(input), checkerEmpty.check(input.thirReturnError))
        }
    }
}
