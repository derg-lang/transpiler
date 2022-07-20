package com.github.derg.transpiler.parser

import com.github.derg.transpiler.util.*

/**
 *
 */
interface Pattern<Type>
{
    fun parse(context: Context): Result<Type, String>
}


/**
 * Specifies a pattern where exactly one of the provided [patterns] must match the context at its current location. The
 * context will resume parsing from the one matching pattern. All other parsing branches are discarded, their contexts
 * are restored.
 */
class PatternOneOf<Type>(private vararg val patterns: Pattern<Type>) : Pattern<Type>
{
    override fun parse(context: Context): Result<Type, String>
    {
        if (!context.hasNext())
            return failureOf("expected token, found end of stream")
        
        // TODO: This context usage is wrong - must support rollback upon failure, must use the same context everywhere
        val results = patterns.map { it.parse(context.copy()) }
        val successes = results.mapNotNull { it.valueOrNull() }
        val failures = results.mapNotNull { it.errorOrNull() }
        
        return when (successes.size)
        {
            0    -> failureOf("no matching pattern: $failures")
            1    -> successOf(successes.single())
            else -> TODO("Only one success is allowed, found " + successes.joinToString { it.toString() })
        }
    }
}

/**
 * Specifies a pattern where all the provided [patterns] must match the context at its current location. The patterns
 * must not necessarily match in any particular order. If any one of the [patterns] do not match the context, the whole
 * sequence is discarded.
 */
class PatternAllOf<Type>(private vararg val patterns: Pair<String, Pattern<Type>>) : Pattern<Map<String, Type>>
{
    override fun parse(context: Context): Result<Map<String, Type>, String>
    {
        if (!context.hasNext())
            return failureOf("expected token, found end of stream")
        
        val remaining = patterns.toMap().toMutableMap()
        val results = mutableMapOf<String, Type>()
        while (remaining.isNotEmpty())
        {
            val (key, parsed) = findLongest(context, remaining)
                .onFailure { return failureOf("no matching pattern: ${it.values}") }
                .valueOrNull() ?: throw IllegalStateException()
            
            results[key] = parsed
            remaining.remove(key)
        }
        return results.toSuccess()
    }
}

/**
 * Specifies a pattern where at least one of the provided [patterns] must match the context at its current location. The
 * pattern which matches the most tokens is selected. The context will resume parsing from the selected matching
 * pattern. All other parsing branches are discarded, their contexts are restored. The longest matching pattern is
 * chosen when parsing the input stream of tokens.
 */
class PatternAnyOf<Type>(private vararg val patterns: Pattern<Type>) : Pattern<Type>
{
    override fun parse(context: Context): Result<Type, String>
    {
        if (!context.hasNext())
            return failureOf("expected token, found end of stream")
        return findLongest(context, patterns).mapError { "no matching pattern: $it" }
    }
}

/**
 * Specifies a pattern where all provided [patterns] must be matched against the context, in the exact same order they
 * appear in. If any one of the [patterns] do not match the context, the whole sequence is discarded.
 */
class PatternSequence<Type>(private vararg val patterns: Pattern<Type>) : Pattern<List<Type>>
{
    override fun parse(context: Context): Result<List<Type>, String>
    {
        if (!context.hasNext())
            return failureOf("expected token, found end of stream")
        
        // TODO: This context usage is wrong - must support rollback upon failure
        return patterns.toList().fold { it.parse(context) }
    }
}

/**
 * Specifies a pattern where the provided [pattern] is optional. The provided pattern is not required to match the
 * context at its current position. If the [pattern] matches the context, parsing continues as ordinary, otherwise the
 * branch is discarded and the context restored, and the [default] is used instead.
 */
class PatternOptional<Type>(private val pattern: Pattern<Type>, private val default: Type) : Pattern<Type>
{
    override fun parse(context: Context): Result<Type, String>
    {
        // TODO: This context usage is wrong - must support rollback upon failure
        return pattern.parse(context).fold(success = { it }, failure = { default }).toSuccess()
    }
}


/**
 * Retrieves the pattern among the provided [patterns] which consumes the longest chain of tokens from the [context].
 * Both the pattern identifier and parsed value is provided, if any pattern did match the context at its current
 * position. If no pattern matched, all errors are returned instead. If multiple patterns tie for longest chain, the
 * parse result is ambiguous and an error must be returned.
 */
private fun <Type, Key> findLongest(
    context: Context,
    patterns: Map<Key, Pattern<Type>>,
): Result<Pair<Key, Type>, Map<Key, String>>
{
    data class Parsed(val key: Key, val outcome: Result<Type, String>, val snapshot: Snapshot)
    
    /** Helper function for parsing a specific [pattern], restoring the context to the [snapshot] afterwards. */
    fun parseThenReset(key: Key, pattern: Pattern<Type>, snapshot: Snapshot): Parsed =
        Parsed(key, pattern.parse(context), context.snapshot()).also { context.restore(snapshot) }
    
    // All patterns must be parsed at the current snapshot location, to determine best fit at this location
    val snapshot = context.snapshot()
    val (successes, failures) = patterns
        .map { parseThenReset(it.key, it.value, snapshot) }
        .partition { it.outcome.isSuccess }
    
    // The outcome may be analyzed, only want the context to resume from the best pattern
    if (successes.isEmpty())
        return failures.associate { it.key to it.outcome.errorOrNull()!! }.toFailure()
    
    val outcome = successes.maxByOrNull { it.snapshot.index }!!
    context.restore(outcome.snapshot)
    return outcome.let { it.key to it.outcome.valueOrNull()!! }.toSuccess()
}

private fun <Type> findLongest(context: Context, patterns: Array<out Pattern<Type>>): Result<Type, List<String>>
{
    return findLongest(context, patterns.mapIndexed { index, pattern -> index to pattern }.toMap())
        .mapValue { it.second }
        .mapError { it.values.toList() }
}
