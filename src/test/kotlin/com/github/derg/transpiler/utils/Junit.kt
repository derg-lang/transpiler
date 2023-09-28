package com.github.derg.transpiler.utils

import org.junit.jupiter.api.Assertions.*

/**
 * Asserts that the [actual] results evaluates to a successful outcome containing the [expected] value.
 */
fun <Type, Error> assertSuccess(expected: Type, actual: Result<Type, Error>, message: String? = null)
{
    assertEquals(expected.toSuccess(), actual, message)
}

/**
 * Asserts that the [actual] results does not evaluate to a successful outcome containing the [expected] value.
 */
fun <Type, Error> assertNotSuccess(expected: Type, actual: Result<Type, Error>, message: String? = null)
{
    assertNotEquals(expected.toSuccess(), actual, message)
}

/**
 * Asserts that the [actual] results evaluates to an unsuccessful outcome containing the [expected] error.
 */
fun <Type, Value> assertFailure(expected: Type, actual: Result<Value, Type>, message: String? = null)
{
    assertEquals(expected.toFailure(), actual, message)
}

/**
 * Asserts that the [actual] results does not evaluate to an unsuccessful outcome containing the [expected] error.
 */
fun <Type, Value> assertNotFailure(expected: Type, actual: Result<Value, Type>, message: String? = null)
{
    assertNotEquals(expected.toFailure(), actual, message)
}
