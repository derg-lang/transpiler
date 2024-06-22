package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.math.*

internal val INT32_MIN = Int.MIN_VALUE.toBigInteger()
internal val INT32_MAX = Int.MAX_VALUE.toBigInteger()
internal val INT64_MIN = Long.MIN_VALUE.toBigInteger()
internal val INT64_MAX = Long.MAX_VALUE.toBigInteger()

/**
 * Converts [this] integer into an int32 if it fits.
 */
internal fun BigInteger.toInt32(): Result<ThirConstInt32, ResolveError>
{
    if (this < INT32_MIN || this > INT32_MAX)
        return ResolveError.InvalidInteger(this).toFailure()
    return ThirConstInt32(toInt()).toSuccess()
}

/**
 * Converts [this] integer into an int64 if it fits.
 */
internal fun BigInteger.toInt64(): Result<ThirConstInt64, ResolveError>
{
    if (this < INT64_MIN || this > INT64_MAX)
        return ResolveError.InvalidInteger(this).toFailure()
    return ThirConstInt64(toLong()).toSuccess()
}
