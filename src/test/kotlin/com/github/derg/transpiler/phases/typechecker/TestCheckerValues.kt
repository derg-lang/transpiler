package com.github.derg.transpiler.phases.typechecker

import com.github.derg.transpiler.phases.typechecker.TypeError.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

class TestCheckerValues
{
    private val bool = thirTypeData(Builtin.BOOL.id)
    private val func = thirTypeCall()
    
    @Nested
    inner class Call
    {
        private val checker = CheckerValue()
    
        @Test
        fun `Given no type, when checking, then correct error`()
        {
            val instance = thirFunOf(value = null, error = null).thirCall()
    
            assertFailure(CallMissingValue(instance), checker.check(instance.thirCall()))
        }
    
        @Test
        fun `Given valid type, when checking, then correct error`()
        {
            val instance = thirFunOf(value = func, error = null).thirCall()
        
            assertSuccess(Unit, checker.check(instance.thirCall()))
        }
        
        @Test
        fun `Given invalid type, when checking, then correct error`()
        {
            val instance = thirFunOf(value = bool, error = null).thirCall()
    
            assertFailure(CallWrongType(instance), checker.check(instance.thirCall()))
        }
    
        @Test
        fun `Given error type, when checking, then correct error`()
        {
            val instance = thirFunOf(value = func, error = bool).thirCall()
        
            assertFailure(CallContainsError(instance), checker.check(instance.thirCall()))
        }
    }
}
