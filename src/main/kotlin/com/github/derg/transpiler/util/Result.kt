package com.github.derg.transpiler.util

/**
 * A result represents the outcome of an operation which may fail. The operation may succeed, in which case the result
 * contains the success [Value], otherwise the failure [Error] value is stored.
 */
sealed class Result<out Value, out Error>
{
    data class Success<Value>(val value: Value) : Result<Value, Nothing>()
    data class Failure<Error>(val error: Error) : Result<Nothing, Error>()
}

fun <Value> Value.toSuccess() = Result.Success(this)
fun <Error> Error.toFailure() = Result.Failure(this)

fun <Value> successOf(value: Value) = value.toSuccess()
fun <Error> failureOf(error: Error) = error.toFailure()

val <Value, Error> Result<Value, Error>.isSuccess: Boolean get() = this is Result.Success<Value>
val <Value, Error> Result<Value, Error>.isFailure: Boolean get() = this is Result.Failure<Error>

fun <Value, Error> Result<Value, Error>.valueOrNull(): Value? = (this as? Result.Success<Value>)?.value
fun <Value, Error> Result<Value, Error>.errorOrNull(): Error? = (this as? Result.Failure<Error>)?.error

/**
 * Transforms the success value using the provided [transformation], if the result represents a success.
 */
fun <Value, Error, T> Result<Value, Error>.map(transformation: (Value) -> T): Result<T, Error> = when (this)
{
    is Result.Success -> transformation(value).toSuccess()
    is Result.Failure -> this
}

/**
 * Transforms the result using either the [success] or [failure] transformations, depending on the result outcome.
 */
fun <Value, Error, T> Result<Value, Error>.fold(success: (Value) -> T, failure: (Error) -> T): T = when (this)
{
    is Result.Success -> success(value)
    is Result.Failure -> failure(error)
}
