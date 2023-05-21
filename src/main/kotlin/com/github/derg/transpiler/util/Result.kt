package com.github.derg.transpiler.util

/**
 * A result represents the outcome of an operation which may fail. The operation may succeed, in which case the result
 * contains the success [Value], otherwise the failure [Error] value is stored.
 */
sealed interface Result<out Value, out Error>
{
    data class Success<Value>(val value: Value) : Result<Value, Nothing>
    data class Failure<Error>(val error: Error) : Result<Nothing, Error>
}

fun <Value> Value.toSuccess() = Result.Success(this)
fun <Error> Error.toFailure() = Result.Failure(this)

fun successOf() = successOf(Unit)
fun failureOf() = failureOf(Unit)
fun <Value> successOf(value: Value) = value.toSuccess()
fun <Error> failureOf(error: Error) = error.toFailure()

val <Value, Error> Result<Value, Error>.isSuccess: Boolean get() = this is Result.Success<Value>
val <Value, Error> Result<Value, Error>.isFailure: Boolean get() = this is Result.Failure<Error>

fun <Value, Error> Result<Value, Error>.valueOrNull(): Value? = (this as? Result.Success<Value>)?.value
fun <Value, Error> Result<Value, Error>.errorOrNull(): Error? = (this as? Result.Failure<Error>)?.error
fun <Value, Error> Result<Value, Error>.valueOrDie(): Value = valueOr { throw IllegalStateException(it.toString()) }

/**
 * Folds the result value such that either the success value is returned, or a value is produced by [function].
 */
inline fun <Value, Error> Result<Value, Error>.valueOr(function: (Error) -> Value): Value = when (this)
{
    is Result.Success -> value
    is Result.Failure -> function(error)
}

/**
 * Invokes the provided [function] only if this result represents a success.
 */
inline fun <Value, Error> Result<Value, Error>.onSuccess(function: (Value) -> Unit): Result<Value, Error>
{
    if (this is Result.Success) function(value)
    return this
}

/**
 * Invokes the provided [function] only if this result represents a failure.
 */
inline fun <Value, Error> Result<Value, Error>.onFailure(function: (Error) -> Unit): Result<Value, Error>
{
    if (this is Result.Failure) function(error)
    return this
}

/**
 * Invokes the [function] only if the result is a success, discarding the success value of that function. If [function]
 * fails, however, the error case is returned.
 */
fun <Value, Error, T> Result<Value, Error>.andThen(function: (Value) -> Result<T, Error>): Result<Value, Error>
{
    if (this is Result.Success) function(value).onFailure { return failureOf(it) }
    return this
}

/**
 * Transforms the success value using the provided [transformation], if the result represents a success.
 */
fun <Value, Error, T> Result<Value, Error>.mapValue(transformation: (Value) -> T): Result<T, Error> = when (this)
{
    is Result.Success -> transformation(value).toSuccess()
    is Result.Failure -> this
}

/**
 * Transforms the failure error using the provided [transformation], if the result represents a failure.
 */
fun <Value, Error, T> Result<Value, Error>.mapError(transformation: (Error) -> T): Result<Value, T> = when (this)
{
    is Result.Success -> this
    is Result.Failure -> transformation(error).toFailure()
}

/**
 * Transforms the result into a different result type only when this result represents a success.
 */
fun <Value, Error, T> Result<Value, Error>.flatMap(transformation: (Value) -> Result<T, Error>): Result<T, Error> =
    when (this)
    {
        is Result.Success -> transformation(value)
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

/**
 * Transforms the collection of failable operations into either all successes, or the first failure case. Each element
 * is transformed using the [transformation] function.
 */
fun <Value, Error, T> Iterable<T>.fold(transformation: (T) -> Result<Value, Error>): Result<List<Value>, Error> =
    map { result -> transformation(result).valueOr { return failureOf(it) } }.toSuccess()

/**
 * Transforms the collection of failable operations into two lists, one containing all the success cases and the other
 * containing all the failure cases.
 */
fun <Value, Error> Iterable<Result<Value, Error>>.partition(): Pair<List<Value>, List<Error>> =
    mapNotNull { it.valueOrNull() } to mapNotNull { it.errorOrNull() }
