package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.*
import java.math.*

class TestCoercion
{
    @Nested
    inner class Int32
    {
        @Test
        fun `Given valid, when converting, then success`()
        {
            assertSuccess(ThirConstInt32(0), BigInteger.ZERO.toInt32())
            assertSuccess(ThirConstInt32(1), BigInteger.ONE.toInt32())
            assertSuccess(ThirConstInt32(Int.MIN_VALUE), INT32_MIN.toInt32())
            assertSuccess(ThirConstInt32(Int.MAX_VALUE), INT32_MAX.toInt32())
        }
        
        @Test
        fun `Given invalid, when converting, then failure`()
        {
            assertFailure(ResolveError.InvalidInteger(INT32_MIN - 1), (INT32_MIN - 1).toInt32())
            assertFailure(ResolveError.InvalidInteger(INT32_MAX + 1), (INT32_MAX + 1).toInt32())
        }
    }
    
    @Nested
    inner class Int64
    {
        @Test
        fun `Given valid, when converting, then success`()
        {
            assertSuccess(ThirConstInt64(0), BigInteger.ZERO.toInt64())
            assertSuccess(ThirConstInt64(1), BigInteger.ONE.toInt64())
            assertSuccess(ThirConstInt64(Long.MIN_VALUE), INT64_MIN.toInt64())
            assertSuccess(ThirConstInt64(Long.MAX_VALUE), INT64_MAX.toInt64())
        }
        
        @Test
        fun `Given invalid, when converting, then failure`()
        {
            assertFailure(ResolveError.InvalidInteger(INT64_MIN - 1), (INT64_MIN - 1).toInt64())
            assertFailure(ResolveError.InvalidInteger(INT64_MAX + 1), (INT64_MAX + 1).toInt64())
        }
    }
}
