package com.github.derg.transpiler.utils

/**
 * A result represents the outcome of an operation which may fail. The operation may succeed, in which case the result
 * contains the success [Value], otherwise the failure [Error] value is stored.
 */
sealed interface Result<out Value, out Error>

data class Success<Value>(val value: Value) : Result<Value, Nothing>
data class Failure<Error>(val error: Error) : Result<Nothing, Error>

fun <Value> Value.toSuccess() = Success(this)
fun <Error> Error.toFailure() = Failure(this)

val <Value, Error> Result<Value, Error>.isSuccess: Boolean get() = this is Success<Value>
val <Value, Error> Result<Value, Error>.isFailure: Boolean get() = this is Failure<Error>

/**
 * Retrieves the value of [this] result, if it is a success. Otherwise, `null` is returned.
 */
fun <Value, Error> Result<Value, Error>.valueOrNull(): Value? = (this as? Success<Value>)?.value

/**
 * Retrieves the value of [this] result, if it is a success. Otherwise, the outcome of the [function] is returned.
 */
inline fun <Value, Error> Result<Value, Error>.valueOr(function: (Error) -> Value): Value = when (this)
{
    is Success -> value
    is Failure -> function(error)
}

/**
 * Retrieves the value of [this] result, if it is a success. Otherwise, an exception is raised.
 */
fun <Value, Error> Result<Value, Error>.valueOrDie(): Value = valueOr { throw IllegalStateException(it.toString()) }

/**
 * Retrieves the error of [this] result, if it is a failure. Otherwise, `null` is returned.
 */
fun <Value, Error> Result<Value, Error>.errorOrNull(): Error? = (this as? Failure<Error>)?.error

/**
 * Retrieves the error of [this] result, if it is a failure. Otherwise, the outcome of the [function] is returned.
 */
inline fun <Value, Error> Result<Value, Error>.errorOr(function: (Value) -> Error): Error = when (this)
{
    is Success -> function(value)
    is Failure -> error
}

/**
 * Invokes the provided [function] only if this result represents a success.
 */
inline fun <Value, Error> Result<Value, Error>.onSuccess(function: (Value) -> Unit): Result<Value, Error>
{
    if (this is Success<Value>) function(value)
    return this
}

/**
 * Invokes the provided [function] only if this result represents a failure.
 */
inline fun <Value, Error> Result<Value, Error>.onFailure(function: (Error) -> Unit): Result<Value, Error>
{
    if (this is Failure<Error>) function(error)
    return this
}

/**
 * Transforms the result using either the [success] or [failure] transformations, depending on the result outcome.
 */
fun <Value, Error, T> Result<Value, Error>.map(success: (Value) -> T, failure: (Error) -> T): T = when (this)
{
    is Success<Value> -> success(value)
    is Failure<Error> -> failure(error)
}

/**
 * Transforms the success value using the provided [transformation], if the result represents a success.
 */
fun <Value, Error, T> Result<Value, Error>.mapValue(transformation: (Value) -> T): Result<T, Error> = when (this)
{
    is Success<Value> -> transformation(value).toSuccess()
    is Failure<Error> -> this
}

/**
 * Transforms the failure error using the provided [transformation], if the result represents a failure.
 */
fun <Value, Error, T> Result<Value, Error>.mapError(transformation: (Error) -> T): Result<Value, T> = when (this)
{
    is Success<Value> -> this
    is Failure<Error> -> transformation(error).toFailure()
}

/**
 * Transforms the result into a different result type only when this result represents a success.
 */
fun <Value, Error, T> Result<Value, Error>.flatMapValue(transformation: (Value) -> Result<T, Error>): Result<T, Error> =
    when (this)
    {
        is Success<Value> -> transformation(value)
        is Failure<Error> -> this
    }

/**
 * Transforms the result into a different result type only when this result represents a success.
 */
fun <Value, Error, T> Result<Value, Error>.flatMapError(transformation: (Error) -> Result<Value, T>): Result<Value, T> =
    when (this)
    {
        is Success<Value> -> this
        is Failure<Error> -> transformation(error)
    }

/**
 * Transforms the collection of fallible operations into either all successes, or the first failure case. Each element
 * is transformed using the [transformation] function.
 */
fun <Value, Error, T> Iterable<T>.mapUntilError(transformation: (T) -> Result<Value, Error>): Result<List<Value>, Error> =
    map { element -> transformation(element).valueOr { return it.toFailure() } }.toSuccess()

/**
 * Transforms the collection of fallible operations into either all successes, or the first failure case. Each element
 * is transformed using the [transformation] function. All values which are mapped to `null` are discarded.
 */
fun <Value : Any, Error, T> Iterable<T>.mapNotInnerNull(transformation: (T) -> Result<Value?, Error>): List<Result<Value, Error>> =
    mapNotNull { element -> transformation(element).map(success = { it?.toSuccess() }, failure = { it.toFailure() }) }

/**
 * Filters the collection using the [predicate] until either all elements have been filtered according to the predicate,
 * or the first failure. If no error occurred, the success case contains all elements which satisfied the predicate.
 */
fun <Error, T> Iterable<T>.filterUntilError(predicate: (T) -> Result<Boolean, Error>): Result<List<T>, Error> =
    filter { element -> predicate(element).valueOr { return it.toFailure() } }.toSuccess()

/**
 * Transforms the collection of fallible operations into two lists, one containing all the success cases and the other
 * containing all the failure cases.
 */
fun <Value, Error> Iterable<Result<Value, Error>>.partitionOutcomes(): Pair<List<Value>, List<Error>> =
    mapNotNull { it.valueOrNull() } to mapNotNull { it.errorOrNull() }
