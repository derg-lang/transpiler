package com.github.derg.transpiler.util

/**
 * Returns the sublist of this iterable starting at the [startIndex] and ending right before the [endIndex].
 *
 * @param startIndex the start index (inclusive).
 * @param endIndex the end index (exclusive).
 */
fun <T> List<T>.sublist(startIndex: Int, endIndex: Int): List<T> = subList(startIndex, endIndex)

/**
 * Returns index of the first element matching the given [predicate] starting from the [startIndex], or -1 if the
 * iterable does not contain such element.
 */
fun <T> List<T>.indexOfFirst(startIndex: Int, predicate: (T) -> Boolean): Int
{
    if (startIndex == 0)
        return indexOfFirst(predicate)
    return sublist(startIndex, size).indexOfFirst(predicate).let { if (it == -1) -1 else startIndex + it }
}

/**
 * Returns index of the last element matching the given [predicate] starting from the [startIndex], or -1 if the
 * iterable does not contain such element.
 */
fun <T> List<T>.indexOfLast(startIndex: Int, predicate: (T) -> Boolean): Int
{
    if (startIndex == 0)
        return indexOfLast(predicate)
    return sublist(startIndex, size).indexOfLast(predicate).let { if (it == -1) -1 else startIndex + it }
}

/**
 * Returns index of the first element matching the given [predicate] starting from the [startIndex], or null if the
 * iterable does not contain any matching element.
 */
fun <T> List<T>.indexOfFirstOrNull(startIndex: Int = 0, predicate: (T) -> Boolean): Int? =
    indexOfFirst(startIndex, predicate).let { if (it == -1) null else it }

/**
 * Returns index of the last element matching the given [predicate] starting from the [startIndex], or null if the
 * iterable does not contain any matching element.
 */
fun <T> List<T>.indexOfLastOrNull(startIndex: Int = 0, predicate: (T) -> Boolean): Int? =
    indexOfLast(startIndex, predicate).let { if (it == -1) null else it }

/**
 * Returns the last non-null value produced by [transform] function being applied to elements of this collection in
 * iteration order, or null if no non-null value was produced.
 */
fun <T, R : Any> List<T>.lastNotNullOfOrNull(transform: (T) -> R?): R? =
    reversed().firstNotNullOfOrNull(transform)

/**
 * Removes and returns the first element matching the given [predicate] starting from the [startIndex], or null if the
 * iterable does not contain any matching element.
 */
fun <T> MutableList<T>.removeFirstOrNull(startIndex: Int = 0, predicate: (T) -> Boolean): T?
{
    val index = indexOfFirstOrNull(startIndex, predicate) ?: return null
    return removeAt(index)
}

/**
 * Removes and returns the last element matching the given [predicate] starting from the [startIndex], or null if the
 * iterable does not contain any matching element.
 */
fun <T> MutableList<T>.removeLastOrNull(startIndex: Int = 0, predicate: (T) -> Boolean): T?
{
    val index = indexOfLastOrNull(startIndex, predicate) ?: return null
    return removeAt(index)
}

/**
 * Removes and returns the first non-null value produced by [transform] function, or null if no non-null value was
 * produced.
 */
fun <T, R : Any> MutableList<T>.removeFirstNotNullOrNull(transform: (T) -> R?): R?
{
    val (value, index) = withIndex().firstNotNullOfOrNull { (i, v) -> transform(v)?.let { it to i } } ?: return null
    removeAt(index)
    return value
}

/**
 * Removes and returns the last non-null value produced by [transform] function, or null if no non-null value was
 * produced.
 */
fun <T, R : Any> MutableList<T>.removeLastNotNullOrNull(transform: (T) -> R?): R?
{
    val (value, index) = withIndex().toList().lastNotNullOfOrNull { (i, v) -> transform(v)?.let { it to i } } ?: return null
    removeAt(index)
    return value
}
