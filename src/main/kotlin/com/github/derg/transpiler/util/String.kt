package com.github.derg.transpiler.util

/**
 * Retrieves the substring spanning from [startIndex] to [endIndex], or if the [endIndex] is beyond the end of the
 * string, the substring spanning from [startIndex].
 */
fun String.substringFrom(startIndex: Int, endIndex: Int): String =
    if (endIndex < length) substring(startIndex, endIndex) else substring(startIndex)

/**
 * Retrieves the index of [char] in the string, starting from [startIndex]. If [char] was not found, null is returned.
 */
fun String.indexOfOrNull(char: Char, startIndex: Int = 0, ignoreCase: Boolean = false): Int? =
    indexOf(char, startIndex, ignoreCase).let { if (it == -1) null else it }

/**
 * Returns index of the first character matching the given [predicate] starting from the [startIndex], or -1 if the
 * string does not contain such character.
 */
fun String.indexOfFirst(startIndex: Int, predicate: (Char) -> Boolean): Int
{
    if (startIndex == 0)
        return indexOfFirst(predicate)
    return substring(startIndex).indexOfFirst(predicate).let { if (it == -1) -1 else startIndex + it }
}

/**
 * Returns index of the first character matching the given [predicate] starting from the [startIndex], or null if the
 * string does not contain such character.
 */
fun String.indexOfFirstOrNull(startIndex: Int = 0, predicate: (Char) -> Boolean): Int? =
    indexOfFirst(startIndex, predicate).let { if (it == -1) null else it }
