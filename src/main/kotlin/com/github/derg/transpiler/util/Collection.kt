package com.github.derg.transpiler.util

/**
 * Splits the original collection into a pair of lists, where first list contains elements for which predicate yielded
 * true, while second list contains elements for which predicate yielded false.
 */
inline fun <K, T> Map<K, T>.partition(predicate: (Map.Entry<K, T>) -> Boolean): Pair<Map<K, T>, Map<K, T>>
{
    val first = mutableMapOf<K, T>()
    val second = mutableMapOf<K, T>()
    
    for (element in this)
    {
        if (predicate(element))
            first[element.key] = element.value
        else
            second[element.key] = element.value
    }
    return Pair(first, second)
}
