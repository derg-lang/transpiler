package com.github.derg.transpiler.utils

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.extension.*
import org.junit.jupiter.params.provider.*
import java.util.stream.*

/**
 * Argument providers are components which provide specific test data to a parameterized test. Various tests may need
 * drastically different data, and some tests work the same across a wide range of inputs. The argument provider may be
 * used to run the same test code on different controllable input.
 *
 * Example usage:
 *
 * ```kotlin
 * private object MyArgumentProvider : ArgumentProvider(1, 2, 3)
 *
 * @ParameterizedTest
 * @ArgumentsSource(MyArgumentProvider::class)
 * fun `Given some data, when doing thing, then expectation happens`(input: Int)
 * {
 *     // Run some test based on the input.
 * }
 * ```
 *
 * @param arguments The arguments which should be passed into the test.
 */
open class ArgumentProvider(vararg arguments: Any) : ArgumentsProvider
{
    private val arguments = arguments.toList()
    
    override fun provideArguments(context: ExtensionContext?): Stream<out Arguments> =
        arguments.map { Arguments.of(it) }.stream()
}

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

/**
 * Asserts that the [actual] value is of the [Expected] type.
 */
inline fun <reified Expected> assertIs(actual: Any)
{
    assertTrue(actual is Expected, "Value '$actual' is not of expected type '${Expected::class.qualifiedName}'")
}
