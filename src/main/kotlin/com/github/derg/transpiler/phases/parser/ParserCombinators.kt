package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.util.*

/**
 * Certain parsers cannot output a single item to represent the products of all parsers involved. Deeply nested parsers
 * results in a bundle of parsers, each which contain their own inner results. To make sense of the final output, all
 * such parsers are grouped together in this parser bundle, providing easy access to each individual parser by their
 * respective keys.
 */
class Parsers(parsers: Map<String, Parser<*>>)
{
    private val items = parsers.mapValues { it.value.produce() }
    
    /**
     * Retrieves the produced item for the parser stored under the given [key].
     */
    @Suppress("UNCHECKED_CAST")
    operator fun <Type> get(key: String): Type = items[key] as Type
}

/**
 * Retrieves all values of all parsers in [this] list under the given [key].
 */
fun <Type> List<Parsers>.produce(key: String): List<Type> = map { it[key] }

/**
 * Parses the token stream in such a way that exactly one of the provided parsers parses. If more than a single pattern
 * matches the context, the parser consuming the most tokens is chosen. Ties are not permitted - exactly one parser must
 * be selected as the longest chain.
 */
class ParserAnyOf<Type>(vararg parsers: Parser<Type>) : Parser<Type>
{
    private val parsers = parsers.mapIndexed { index, parser -> index to parser }
    private val outcomes = mutableMapOf<Int, ParseOk>()
    private val disqualified = mutableSetOf<Int>()
    
    override fun skipable(): Boolean = parsers.any { it.second.skipable() }
    override fun produce(): Type = parsers.single { it.first !in disqualified }.second.produce()
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        outcomes.clear()
        parsers.ifEmpty { return successOf(ParseOk.Finished) }
        parsers.filter { it.first !in disqualified }.map { parse(token, it.first, it.second) }
        
        val candidates = parsers.filter { it.first !in disqualified }
        val (finished, unfinished) = candidates.partition { outcomes[it.first] == ParseOk.Finished }
        
        candidates.ifEmpty { return failureOf(ParseError.UnexpectedToken(token)) }
        unfinished.ifEmpty { return successOf(ParseOk.Finished) } // TODO: Ties must be considered errors here
        finished.forEach { disqualified.add(it.first) }
        
        return if (unfinished.any { outcomes[it.first] == ParseOk.Complete })
            successOf(ParseOk.Complete)
        else
            successOf(ParseOk.Incomplete)
    }
    
    private fun parse(token: Token, index: Int, parser: Parser<Type>): Result<ParseOk, ParseError> =
        parser.parse(token).onSuccess { outcomes[index] = it }.onFailure { disqualified.add(index) }
    
    override fun reset()
    {
        parsers.forEach { it.second.reset() }
        disqualified.clear()
    }
}

/**
 * Parses the token stream in such a way that all the provided parsers parses. The parses are parsed in order, where the
 * parser which consumes the most tokens is selected first.
 */
class ParserAllOf(vararg parsers: Pair<String, Parser<*>>) : Parser<Parsers>
{
    private val parsers = parsers.toList()
    private val finished = mutableSetOf<String>()
    private val disqualified = mutableSetOf<String>()
    
    override fun skipable(): Boolean = parsers.all { it.second.skipable() }
    override fun produce(): Parsers = Parsers(parsers.toMap())
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        val remaining = parsers.filter { it.first !in finished }.ifEmpty { return successOf(ParseOk.Finished) }
        val outcomes = remaining.filter { it.first !in disqualified }.associate { it.first to it.second.parse(token) }
        
        val (successes, failures) = outcomes.partition { it.value.isSuccess }
        val (finished, unfinished) = successes.partition { it.value.valueOrNull() == ParseOk.Finished }
        
        successes.ifEmpty { return failureOf(ParseError.UnexpectedToken(token)) }
        failures.forEach { disqualified.add(it.key) }
        
        if (unfinished.isNotEmpty())
        {
            finished.forEach { disqualified.add(it.key) }
            
            val skipable = remaining.filter { it.first !in unfinished }.all { it.second.skipable() }
            val isIncomplete = !skipable || unfinished.any { it.value.valueOrNull() == ParseOk.Incomplete }
            return if (isIncomplete) successOf(ParseOk.Incomplete) else successOf(ParseOk.Complete)
        }
        
        val survivor = remaining.singleOrNull { it.first in finished }
            ?: return failureOf(ParseError.UnexpectedToken(token))
        
        parsers.filter { it.first in disqualified }.forEach { it.second.reset() }
        disqualified.clear()
        this.finished.add(survivor.first)
        return parse(token)
    }
    
    override fun reset()
    {
        parsers.forEach { it.second.reset() }
        finished.clear()
        disqualified.clear()
    }
}

/**
 * Parses the token stream in such a way that all the provided parsers parses, in the exact same order they are
 * specified. Once the first parser is finished, the next parser in the sequence will parse the next tokens.
 */
class ParserSequence(vararg parsers: Pair<String, Parser<*>>) : Parser<Parsers>
{
    private val parsers = parsers.toList()
    private var index = 0
    
    override fun skipable(): Boolean = parsers.all { it.second.skipable() }
    override fun produce(): Parsers = Parsers(parsers.toMap())
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        val current = parsers.getOrNull(index)?.second ?: return successOf(ParseOk.Finished)
        val outcome = current.parse(token).valueOr { return failureOf(it) }
        if (outcome == ParseOk.Finished)
            return if (++index >= parsers.size) successOf(ParseOk.Finished) else parse(token)
        
        val skipable = (index + 1 until parsers.size).all { parsers[it].second.skipable() }
        val isIncomplete = !skipable || outcome == ParseOk.Incomplete
        return if (isIncomplete) successOf(ParseOk.Incomplete) else successOf(ParseOk.Complete)
    }
    
    override fun reset()
    {
        parsers.forEach { it.second.reset() }
        index = 0
    }
}

/**
 * Parses the token stream where the [parser] is parsed as many times as it possibly can be. Every value which was
 * extracted from the [parser] will be present in the final output list. Every value is required to be separated by the
 * [separator]. The [separator] may also optionally appear at the very end of the sequence (i.e. a trailing comma).
 */
class ParserRepeating<Type>(private val parser: Parser<Type>, private val separator: Parser<*>? = null) :
    Parser<List<Type>>
{
    private val values = mutableListOf<Type>()
    private var isSeparator = false
    private var isIncomplete = false
    
    override fun skipable(): Boolean = true
    override fun produce(): List<Type> = values.toList() // Returns *copy* of list
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        val current = if (isSeparator && separator != null) separator else parser
        val outcome = current.parse(token)
            .valueOr { return if (isIncomplete) failureOf(it) else successOf(ParseOk.Finished) }
        if (outcome == ParseOk.Finished)
            return swapAndParse(token, current)
        
        isIncomplete = outcome == ParseOk.Incomplete
        return if (isIncomplete) successOf(ParseOk.Incomplete) else successOf(ParseOk.Complete)
    }
    
    private fun swapAndParse(token: Token, current: Parser<*>): Result<ParseOk, ParseError>
    {
        if (!isSeparator)
            parser.produce().let { values.add(it) }
        
        current.reset()
        isIncomplete = false
        isSeparator = !isSeparator && separator != null
        return parse(token)
    }
    
    override fun reset()
    {
        parser.reset()
        separator?.reset()
        values.clear()
        isSeparator = false
        isIncomplete = false
    }
}

/**
 * Parses the token stream in such a way that the parser is considered optional. If the parser does not accept the first
 * token, the parser is considered satisfied and will not require additional tokens. However, if the parser has accepted
 * any tokens, the parser must be provided enough tokens to satisfy the wrapped parser.
 *
 * If the parser could not locate any suitable tokens, when producing the outcome, the [default] will be returned
 * instead.
 */
class ParserOptional<Type>(private val parser: Parser<Type>, private val default: Type? = null) : Parser<Type?>
{
    private var isParsing = false
    private var isOngoing = false
    
    override fun skipable(): Boolean = true
    override fun produce(): Type? = if (isOngoing) parser.produce() else if (isParsing) default else null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        isParsing = true
        val outcome = parser.parse(token)
            .valueOr { return if (isOngoing) failureOf(it) else successOf(ParseOk.Finished) }
        isOngoing = true
        return successOf(outcome)
    }
    
    override fun reset()
    {
        parser.reset()
        isParsing = false
        isOngoing = false
    }
}

/**
 * Parses the token stream according to the provided pattern [factory]. When the pattern is considered satisfied, this
 * parser is also satisfied and may use the [mapper] to produce the final output. This parser may be used recursively as
 * well, i.e. allowing an expression parser to parse another expression while in the middle of parsing.
 */
class ParserPattern<Type, Out>(private val factory: () -> Parser<Out>, private val mapper: (Out) -> Type) :
    Parser<Type>
{
    private var parser: Parser<Out>? = null
    
    /**
     * Actually instantiates the parser - if the parser has already been instantiated, the same instance is used for
     * further token processing.
     */
    private fun parser(): Parser<Out> = parser ?: factory().also { parser = it }
    
    override fun skipable(): Boolean = parser().skipable()
    override fun produce(): Type = mapper(parser().produce())
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser().parse(token)
    override fun reset()
    {
        parser = null
    }
}
