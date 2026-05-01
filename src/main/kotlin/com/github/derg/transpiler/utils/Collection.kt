package com.github.derg.transpiler.utils

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

/**
 * Generates a list with [this] as the only element.
 */
fun <Type> Type.asList(): List<Type> = listOf(this)

/**
 * Removes all elements of type [Type] from the collection, returning all elements which were removed.
 */
inline fun <reified Type> MutableCollection<*>.removeInstance(): List<Type>
{
    val collection = mutableListOf<Type>()
    val iterator = iterator()
    while (iterator.hasNext())
    {
        val entry = iterator.next()
        if (entry !is Type)
            continue
        
        iterator.remove()
        collection += entry
    }
    return collection
}
