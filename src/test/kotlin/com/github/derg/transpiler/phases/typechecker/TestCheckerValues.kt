package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.phases.typechecker.TypeError.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*

class TestCheckerValues
{
    private val bool = thirTypeVar(Builtin.BOOL.id)
    private val func = thirTypeFun()
    
    @Nested
    inner class Call
    {
        private val checker = CheckerValue()
        
        @Test
        fun `Given no instance type, when checking, then correct error`()
        {
            val instance = thirFunOf(value = null, error = null).thirCall()
            
            assertFailure(CallMissingValue(instance), checker.check(instance.thirCall()))
        }
        
        @Test
        fun `Given valid instance type, when checking, then correct error`()
        {
            val instance = thirFunOf(value = func, error = null).thirCall()
            
            assertSuccess(Unit, checker.check(instance.thirCall()))
        }
        
        @Test
        fun `Given invalid instance type, when checking, then correct error`()
        {
            val instance = thirFunOf(value = bool, error = null).thirCall()
            
            assertFailure(CallWrongType(instance), checker.check(instance.thirCall()))
        }
        
        @Test
        fun `Given error instance type, when checking, then correct error`()
        {
            val instance = thirFunOf(value = func, error = bool).thirCall()
            
            assertFailure(CallContainsError(instance), checker.check(instance.thirCall()))
        }
        
        @Test
        fun `Given valid instance value, when checking, then correct outcome`()
        {
            val instance = thirFunOf(value = thirTypeFun(func), error = null).thirCall()
            val input = instance.thirCall(value = func, error = null)
            
            assertSuccess(Unit, checker.check(input.thirCall()))
        }
        
        @Test
        fun `Given invalid instance value, when checking, then correct error`()
        {
            val instance = thirFunOf(value = thirTypeFun(func), error = bool).thirCall()
            val input = instance.thirCall(value = func, error = null)
            
            assertFailure(CallContainsError(instance), checker.check(input.thirCall()))
        }
        
        @Test
        fun `Given no parameter error, when checking, then correct outcome`()
        {
            val function = thirFunOf(params = listOf(thirParamOf(type = bool)))
            val instance = thirFunOf(value = null, error = null).thirCall()
            
            assertSuccess(Unit, checker.check(function.thirCall(instance)))
        }
        
        @Test
        fun `Given error parameter type, when checking, then correct error`()
        {
            val function = thirFunOf(params = listOf(thirParamOf(type = bool)))
            val instance = thirFunOf(value = null, error = bool).thirCall()
            
            assertFailure(CallContainsError(instance), checker.check(function.thirCall(instance)))
        }
    }
    
    @Nested
    inner class Catch
    {
        private val checker = CheckerValue()
        
        private val neither = thirFunOf(value = null, error = null).thirCall()
        private val value = thirFunOf(value = bool, error = null).thirCall()
        private val error = thirFunOf(value = null, error = bool).thirCall()
        private val both = thirFunOf(value = bool, error = bool).thirCall()
        
        @Test
        fun `Given lhs raise, when checking, then correct outcome`()
        {
            assertFailure(CatchMissingError(neither), checker.check(neither thirCatchRaise 0))
            assertFailure(CatchMissingError(value), checker.check(value thirCatchRaise 0))
            assertSuccess(Unit, checker.check(error thirCatchRaise 0))
            assertSuccess(Unit, checker.check(both thirCatchRaise 0))
        }
        
        @Test
        fun `Given lhs return, when checking, then correct outcome`()
        {
            assertFailure(CatchMissingError(neither), checker.check(neither thirCatchReturn 0))
            assertFailure(CatchMissingError(value), checker.check(value thirCatchReturn 0))
            assertSuccess(Unit, checker.check(error thirCatchReturn 0))
            assertSuccess(Unit, checker.check(both thirCatchReturn 0))
        }
        
        @Test
        fun `Given lhs handle, when checking, then correct outcome`()
        {
            assertFailure(CatchMissingValue(neither), checker.check(neither thirCatchHandle 0))
            assertFailure(CatchMissingError(value), checker.check(value thirCatchHandle 0))
            assertFailure(CatchMissingValue(error), checker.check(error thirCatchHandle 0))
            assertSuccess(Unit, checker.check(both thirCatchHandle 0))
        }
        
        @Test
        fun `Given rhs raise, when checking, then correct outcome`()
        {
            assertFailure(CatchMissingValue(neither), checker.check(both thirCatchRaise neither))
            assertSuccess(Unit, checker.check(both thirCatchRaise value))
            assertFailure(CatchMissingValue(error), checker.check(both thirCatchRaise error))
            assertFailure(CatchContainsError(both), checker.check(both thirCatchRaise both))
        }
        
        @Test
        fun `Given rhs return, when checking, then correct outcome`()
        {
            assertFailure(CatchMissingValue(neither), checker.check(both thirCatchReturn neither))
            assertSuccess(Unit, checker.check(both thirCatchReturn value))
            assertFailure(CatchMissingValue(error), checker.check(both thirCatchReturn error))
            assertFailure(CatchContainsError(both), checker.check(both thirCatchReturn both))
        }
        
        @Test
        fun `Given rhs handle, when checking, then correct outcome`()
        {
            assertFailure(CatchMissingValue(neither), checker.check(both thirCatchHandle neither))
            assertSuccess(Unit, checker.check(both thirCatchHandle value))
            assertFailure(CatchMissingValue(error), checker.check(both thirCatchHandle error))
            assertFailure(CatchContainsError(both), checker.check(both thirCatchHandle both))
        }
        
        @Test
        fun `Given valid lhs value, when checking, then correct outcome`()
        {
            val instance = thirFunOf(value = thirTypeFun(bool), error = null).thirCall()
            val input = instance.thirCall(value = bool, error = bool)
            
            assertSuccess(Unit, checker.check(input thirCatchRaise 0))
            assertSuccess(Unit, checker.check(input thirCatchReturn 0))
            assertSuccess(Unit, checker.check(input thirCatchHandle 0))
        }
        
        @Test
        fun `Given invalid lhs value, when checking, then correct error`()
        {
            val instance = thirFunOf(value = thirTypeFun(bool), error = bool).thirCall()
            val input = instance.thirCall(value = bool, error = bool)
            
            assertFailure(CallContainsError(instance), checker.check(input thirCatchRaise 0))
            assertFailure(CallContainsError(instance), checker.check(input thirCatchReturn 0))
            assertFailure(CallContainsError(instance), checker.check(input thirCatchHandle 0))
        }
        
        @Test
        fun `Given valid rhs value, when checking, then correct outcome`()
        {
            val instance = thirFunOf(value = thirTypeFun(bool), error = null).thirCall()
            val input = instance.thirCall(value = bool, error = null)
            
            assertSuccess(Unit, checker.check(both thirCatchRaise input))
            assertSuccess(Unit, checker.check(both thirCatchReturn input))
            assertSuccess(Unit, checker.check(both thirCatchHandle input))
        }
        
        @Test
        fun `Given invalid rhs value, when checking, then correct error`()
        {
            val instance = thirFunOf(value = thirTypeFun(bool), error = bool).thirCall()
            val input = instance.thirCall(value = bool, error = null)
            
            assertFailure(CallContainsError(instance), checker.check(both thirCatchRaise input))
            assertFailure(CallContainsError(instance), checker.check(both thirCatchReturn input))
            assertFailure(CallContainsError(instance), checker.check(both thirCatchHandle input))
        }
    }
}
