package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.phases.typechecker.TypeError.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

class TestCheckerInstructions
{
    private val bool = thirTypeData(Builtin.BOOL.id)
    private val int32 = thirTypeData(Builtin.INT32.id)
    
    @Nested
    inner class Assign
    {
        private val checker = CheckerInstruction(value = bool, error = null)
        private val variable = thirVarOf(type = bool)
        
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = bool).thirCall()
            
            assertSuccess(Unit, checker.check(variable.thirAssign(input)))
        }
        
        @Test
        fun `Given invalid type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData()).thirCall()
            
            assertFailure(AssignWrongType(input), checker.check(variable.thirAssign(input)))
        }
        
        @Test
        fun `Given error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = bool, error = bool).thirCall()
            
            assertFailure(AssignContainsError(input), checker.check(variable.thirAssign(input)))
        }
        
        @Test
        fun `Given no type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null).thirCall()
            
            assertFailure(AssignMissingValue(input), checker.check(variable.thirAssign(input)))
        }
    
        @Test
        fun `Given valid value, when checking, then correct outcome`()
        {
            val instance = thirFunOf(value = thirTypeCall(bool), error = null).thirCall()
            val input = instance.thirCall(value = bool, error = null)
            
            assertSuccess(Unit, checker.check(variable.thirAssign(input)))
        }
        
        @Test
        fun `Given invalid value, when checking, then correct error`()
        {
            val instance = thirFunOf(value = thirTypeCall(bool), error = int32).thirCall()
            val input = instance.thirCall(value = bool, error = null)
    
            assertFailure(CallContainsError(instance), checker.check(variable.thirAssign(input)))
        }
    }
    
    @Nested
    inner class Branch
    {
        private val checker = CheckerInstruction(value = null, error = null)
        
        @Test
        fun `Given valid type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = bool).thirCall()
            
            assertSuccess(Unit, checker.check(input.thirBranch()))
        }
        
        @Test
        fun `Given invalid type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData()).thirCall()
            
            assertFailure(BranchWrongType(input), checker.check(input.thirBranch()))
        }
        
        @Test
        fun `Given error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = bool, error = bool).thirCall()
            
            assertFailure(BranchContainsError(input), checker.check(input.thirBranch()))
        }
        
        @Test
        fun `Given no type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null).thirCall()
            
            assertFailure(BranchMissingValue(input), checker.check(input.thirBranch()))
        }
    
        @Test
        fun `Given valid value, when checking, then correct outcome`()
        {
            val instance = thirFunOf(value = thirTypeCall(bool), error = null).thirCall()
            val input = instance.thirCall(value = bool, error = null)
        
            assertSuccess(Unit, checker.check(input.thirBranch()))
        }
    
        @Test
        fun `Given invalid value, when checking, then correct error`()
        {
            val instance = thirFunOf(value = thirTypeCall(bool), error = int32).thirCall()
            val input = instance.thirCall(value = bool, error = null)
        
            assertFailure(CallContainsError(instance), checker.check(input.thirBranch()))
        }
    }
    
    @Nested
    inner class Evaluate
    {
        private val checker = CheckerInstruction(value = null, error = null)
        
        @Test
        fun `Given no type, when checking, then correct outcome`()
        {
            val input = thirFunOf(value = null, error = null).thirCall()
            
            assertSuccess(Unit, checker.check(input.thirEval))
        }
        
        @Test
        fun `Given value type, when checking, then correct error`()
        {
            val input = thirFunOf(value = thirTypeData(), error = null).thirCall()
            
            assertFailure(EvaluateContainsValue(input), checker.check(input.thirEval))
        }
        
        @Test
        fun `Given error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null, error = thirTypeData()).thirCall()
            
            assertFailure(EvaluateContainsError(input), checker.check(input.thirEval))
        }
    
        @Test
        fun `Given valid value, when checking, then correct outcome`()
        {
            val instance = thirFunOf(value = thirTypeCall(), error = null).thirCall()
            val input = instance.thirCall(value = null, error = null)
        
            assertSuccess(Unit, checker.check(input.thirEval))
        }
    
        @Test
        fun `Given invalid value, when checking, then correct error`()
        {
            val instance = thirFunOf(value = thirTypeCall(), error = int32).thirCall()
            val input = instance.thirCall(value = null, error = null)
        
            assertFailure(CallContainsError(instance), checker.check(input.thirEval))
        }
    }
    
    @Nested
    inner class Return
    {
        private val checkerEmpty = CheckerInstruction(value = null, error = null)
        private val checkerValue = CheckerInstruction(value = bool, error = null)
        private val checkerError = CheckerInstruction(value = null, error = bool)
        
        @Test
        fun `Given no type, when checking, then correct outcome`()
        {
            assertSuccess(Unit, checkerEmpty.check(ThirReturn))
        }
        
        @Test
        fun `Given error type, when checking, then correct outcome`()
        {
            assertSuccess(Unit, checkerError.check(ThirReturn))
        }
        
        @Test
        fun `Given value type, when checking, then correct error`()
        {
            assertFailure(ReturnMissingExpression, checkerValue.check(ThirReturn))
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
        fun `Given invalid type, when checking, then correct error`()
        {
            val input = thirFunOf(value = int32).thirCall()
            
            assertFailure(ReturnWrongType(input), checkerValue.check(input.thirReturnValue))
        }
        
        @Test
        fun `Given error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = bool, error = bool).thirCall()
            
            assertFailure(ReturnContainsError(input), checkerValue.check(input.thirReturnValue))
        }
        
        @Test
        fun `Given no type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null).thirCall()
            
            assertFailure(ReturnMissingValue(input), checkerValue.check(input.thirReturnValue))
        }
        
        @Test
        fun `Given forbidden return, when checking, then correct error`()
        {
            val input = thirFunOf(value = bool).thirCall()
            
            assertFailure(ReturnContainsValue(input), checkerEmpty.check(input.thirReturnValue))
        }
    
        @Test
        fun `Given valid value, when checking, then correct outcome`()
        {
            val instance = thirFunOf(value = thirTypeCall(bool), error = null).thirCall()
            val input = instance.thirCall(value = bool, error = null)
        
            assertSuccess(Unit, checkerValue.check(input.thirReturnValue))
        }
    
        @Test
        fun `Given invalid value, when checking, then correct error`()
        {
            val instance = thirFunOf(value = thirTypeCall(bool), error = int32).thirCall()
            val input = instance.thirCall(value = bool, error = null)
        
            assertFailure(CallContainsError(instance), checkerValue.check(input.thirReturnValue))
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
        fun `Given invalid type, when checking, then correct error`()
        {
            val input = thirFunOf(value = int32).thirCall()
            
            assertFailure(ReturnWrongType(input), checkerError.check(input.thirReturnError))
        }
        
        @Test
        fun `Given error type, when checking, then correct error`()
        {
            val input = thirFunOf(value = bool, error = bool).thirCall()
            
            assertFailure(ReturnContainsError(input), checkerError.check(input.thirReturnError))
        }
        
        @Test
        fun `Given no type, when checking, then correct error`()
        {
            val input = thirFunOf(value = null).thirCall()
            
            assertFailure(ReturnMissingValue(input), checkerError.check(input.thirReturnError))
        }
        
        @Test
        fun `Given forbidden return, when checking, then correct error`()
        {
            val input = thirFunOf(value = bool).thirCall()
            
            assertFailure(ReturnContainsValue(input), checkerEmpty.check(input.thirReturnError))
        }
    
        @Test
        fun `Given valid value, when checking, then correct outcome`()
        {
            val instance = thirFunOf(value = thirTypeCall(bool), error = null).thirCall()
            val input = instance.thirCall(value = bool, error = null)
        
            assertSuccess(Unit, checkerError.check(input.thirReturnError))
        }
    
        @Test
        fun `Given invalid value, when checking, then correct error`()
        {
            val instance = thirFunOf(value = thirTypeCall(bool), error = int32).thirCall()
            val input = instance.thirCall(value = bool, error = null)
        
            assertFailure(CallContainsError(instance), checkerError.check(input.thirReturnError))
        }
    }
}
