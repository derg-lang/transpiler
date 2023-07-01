package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.phases.lexer.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.util.*
import org.junit.jupiter.api.Assertions.*

/**
 * Converts [this] value into a literal expression if possible. The expression can only be generated from numeric,
 * boolean, and string values.
 */
fun Any.toExp(type: Name? = null): AstExpression = when (this)
{
    is Boolean       -> AstBool(this)
    is Double        -> AstReal(toBigDecimal(), type)
    is Float         -> AstReal(toBigDecimal(), type)
    is Int           -> AstReal(toBigDecimal(), type)
    is String        -> AstText(this, type)
    is AstExpression -> this
    else             -> throw IllegalStateException("Cannot convert '$this' to an expression")
}

// Various primitive conversion functions
fun Name.toVar() = AstRead(this)
fun Name.toFun(vararg arguments: AstArgument) = AstCall(this, arguments.toList())
fun Name.toSub(vararg arguments: AstArgument) = AstSubscript(this, arguments.toList())
fun Any.toArg(name: Name? = null) = AstArgument(name, toExp())

/**
 * To simplify testing of the parsing of source code for any particular pattern factory, a helper class is provided.
 * This tester class allows the developer to write clearer and more concise test cases when parsing specific source
 * code. Each source code snippet can be tailor-made to suit certain edge-cases.
 */
class Tester<Type>(factory: () -> Parser<Type>)
{
    internal val parser: Parser<Type> = factory()
    private var tokens: List<Token> = emptyList()
    private var cursor: Int = 0
    
    /**
     * Parses the input [source] code and stores the tokens and the context for further analysis.
     */
    fun parse(source: String): Tester<Type>
    {
        parser.reset()
        tokens = tokenize(source).map { it.data }
        cursor = 0
        return this
    }
    
    /**
     * Retrieves the next token in the token stream, if there are any remaining.
     */
    private fun next(): Token = tokens.getOrNull(cursor++) ?: EndOfFile
    
    /**
     * Proceeds with parsing the [count] next tokens. All outcomes are expected to be successes.
     */
    fun step(count: Int): Tester<Type>
    {
        repeat(count) { assertTrue(parser.parse(next()).isSuccess, "iteration ${it + 1}") }
        return this
    }
    
    /**
     * The next [count] of tokens are all expected to succeed parsing, and must parse with an incomplete state.
     */
    fun isWip(count: Int): Tester<Type>
    {
        repeat(count) { assertEquals(successOf(ParseOk.Incomplete), parser.parse(next()), "iteration ${it + 1}") }
        return this
    }
    
    /**
     * The next [count] of tokens are all expected to succeed parsing, and must parse with a complete state.
     */
    fun isOk(count: Int): Tester<Type>
    {
        repeat(count) { assertEquals(successOf(ParseOk.Complete), parser.parse(next()), "iteration ${it + 1}") }
        return this
    }
    
    /**
     * The parser is expected to have produced the given [value] already.
     */
    fun isValue(value: Type): Tester<Type>
    {
        assertEquals(value, parser.produce())
        return this
    }
    
    /**
     * The parser is expected to have finished producing its item, and any subsequent parsing operations will not make
     * any difference anymore.
     */
    fun isDone(): Tester<Type>
    {
        assertEquals(successOf(ParseOk.Finished), parser.parse(EndOfFile))
        return this
    }
    
    /**
     * Expects the parsing to fail due to the given [error] when parsing the next token.
     */
    fun isBad(function: (List<Token>) -> ParseError): Tester<Type>
    {
        assertEquals(failureOf(function(tokens)), parser.parse(next()))
        return this
    }
    
    /**
     * The parser must be reverted to its original state once reset.
     */
    fun resets(): Tester<Type>
    {
        parser.reset()
        
        val produced = try
        {
            parser.produce()
        }
        catch (e: IllegalStateException)
        {
            null
        }
        catch (e: IllegalArgumentException)
        {
            null
        }
        
        assertTrue(produced == null || produced == emptyList<Any>())
        return this
    }
}

/**
 * The parser is expected to have produced the given [value] already and stored it under the given [key].
 */
fun <Type, Bundle : Parsers?> Tester<Bundle>.hasValue(key: String, value: Type): Tester<Bundle>
{
    assertEquals(value, parser.produce()?.get(key))
    return this
}

/**
 * The parser is expected to have produced the given [values] already and stored it under the given [key]. The parser
 * must be a deeply nested object - that is, a [Parsers] bundle containing other [Parsers].
 */
fun <Type, Bundle : Parsers?> Tester<Bundle>.hasValue(key: String, vararg values: Pair<String, Type>): Tester<Bundle>
{
    val parsers = parser.produce()?.get<Parsers>(key)
    assertEquals(values.map { it.second }, values.mapNotNull { parsers?.get(it.first) })
    return this
}

/**
 * The parser is expected to have produced the given [values] already and stored them under the given [key].
 */
fun <Type> Tester<List<Parsers>>.hasValue(key: String, vararg values: Type): Tester<List<Parsers>>
{
    assertEquals(values.toList(), parser.produce().produce<Type>(key))
    return this
}
