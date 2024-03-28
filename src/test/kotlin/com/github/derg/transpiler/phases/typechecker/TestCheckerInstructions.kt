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
        private val checker = CheckerInstruction(value = null, error = null)
        
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = thirTypeData(Builtin.BOOL.id), error = null).thirCall()
            
            assertSuccess(Unit, checker.check(input.thirBranch()))
        }
        
        @Test
        fun `Given invalid value type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData(), error = null).thirCall()
            val expected = TypeError.BranchPredicateNotBool(input)
            
            assertFailure(expected, checker.check(input.thirBranch()))
        }
        
        @Test
        fun `Given invalid error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData(Builtin.BOOL.id), error = thirTypeData()).thirCall()
            val expected = TypeError.BranchPredicateHasError(input)
            
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
            val expected = TypeError.EvaluateHasValue(input)
            
            assertFailure(expected, checker.check(input.thirEval))
        }
        
        @Test
        fun `Given invalid error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null, error = thirTypeData()).thirCall()
            val expected = TypeError.EvaluateHasError(input)
            
            assertFailure(expected, checker.check(input.thirEval))
        }
    }
    
    @Nested
    inner class Return
    {
        private val checkerEmpty = CheckerInstruction(value = null, error = null)
        private val checkerValue = CheckerInstruction(value = thirTypeData(Builtin.BOOL.id), error = null)
        private val checkerError = CheckerInstruction(value = null, error = thirTypeData(Builtin.BOOL.id))
        
        @Test
        fun `Given no types, when checking, then correct outcome`()
        {
            assertSuccess(Unit, checkerEmpty.check(ThirReturn))
        }
        
        @Test
        fun `Given value type, when checking, then correct error`()
        {
            assertFailure(TypeError.ReturnLacksValue, checkerValue.check(ThirReturn))
        }
        
        @Test
        fun `Given error type, when checking, then correct error`()
        {
            assertFailure(TypeError.ReturnLacksError, checkerError.check(ThirReturn))
        }
    }
    
    @Nested
    inner class ReturnValue
    {
        private val checkerEmpty = CheckerInstruction(value = null, error = null)
        private val checkerValue = CheckerInstruction(value = thirTypeData(Builtin.BOOL.id), error = null)
        
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val value = thirFunOf(value = thirTypeData(Builtin.BOOL.id)).thirCall()
            
            assertSuccess(Unit, checkerValue.check(value.thirReturnValue))
        }
        
        @Test
        fun `Given no type, when checking, then correct error`()
        {
            val value = thirFunOf(value = null).thirCall()
            
            assertFailure(TypeError.ReturnHasValue(value), checkerEmpty.check(value.thirReturnValue))
        }
        
        @Test
        fun `Given invalid type, when checking, then correct error`()
        {
            val value = thirFunOf(value = thirTypeData(Builtin.INT32.id)).thirCall()
            
            assertFailure(TypeError.ReturnWrongValue(value), checkerValue.check(value.thirReturnValue))
        }
    }
    
    @Nested
    inner class ReturnError
    {
        private val checkerEmpty = CheckerInstruction(value = null, error = null)
        private val checkerError = CheckerInstruction(value = null, error = thirTypeData(Builtin.BOOL.id))
        
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val error = thirFunOf(error = thirTypeData(Builtin.BOOL.id)).thirCall()
            
            assertSuccess(Unit, checkerError.check(error.thirReturnError))
        }
        
        @Test
        fun `Given no type, when checking, then correct error`()
        {
            val error = thirFunOf(error = null).thirCall()
            
            assertFailure(TypeError.ReturnHasError(error), checkerEmpty.check(error.thirReturnError))
        }
        
        @Test
        fun `Given invalid type, when checking, then correct error`()
        {
            val error = thirFunOf(error = thirTypeData(Builtin.INT32.id)).thirCall()
            
            assertFailure(TypeError.ReturnWrongError(error), checkerError.check(error.thirReturnError))
        }
    }
}
