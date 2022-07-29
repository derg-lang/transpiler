package com.github.derg.transpiler.parser

import com.github.derg.transpiler.ast.Access
import com.github.derg.transpiler.ast.Parameter
import com.github.derg.transpiler.ast.Value
import com.github.derg.transpiler.lexer.Token
import com.github.derg.transpiler.lexer.tokenize
import com.github.derg.transpiler.util.successOf
import com.github.derg.transpiler.util.toFailure
import kotlin.test.assertEquals

internal val Boolean.value: Value.Bool get() = Value.Bool(this)
internal val Int.value: Value.Real get() = Value.Real(toBigDecimal(), null)
internal val Int.parameter: Parameter get() = Parameter(null, value)
internal val String.value: Value.Text get() = Value.Text(this, null)
internal val String.variable: Access.Variable get() = Access.Variable(this)
internal val String.function: Access.Function get() = Access.Function(this, emptyList())

/**
 * To simplify testing of the parsing of source code for any particular pattern [factory], a helper class is provided.
 * This tester class allows the developer to write clearer and more concise test cases when parsing specific source
 * code. Each source code snippet can be tailor-made to suit certain edge-cases.
 */
class PatternTester<Type>(private val factory: () -> Parser<Type>)
{
    private lateinit var parser: Parser<Type>
    private lateinit var context: Context
    private lateinit var tokens: List<Token>
    
    /**
     * Parses the input [source] code and stores the tokens and the context for further analysis.
     */
    fun parse(source: String): PatternTester<Type>
    {
        parser = factory()
        tokens = tokenize(source).map { it.data }
        context = Context(tokens)
        return this
    }
    
    /**
     * Expects the parsing to succeed. Whenever parsing succeeds, the context cursor is moved forwards any number of
     * steps, although when a specific pattern succeeds, the cursor is expected to land on a known [index].
     */
    fun isGood(index: Int, value: Type): PatternTester<Type>
    {
        assertEquals(successOf(value), parser.parse(context))
        assertEquals(index, context.snapshot())
        return this
    }
    
    /**
     * Expects the parsing to fail due to the given [error]. Whenever parsing fails, the context is not permitted to
     * move forwards; its cursor must remain fixed.
     */
    fun isBad(function: (List<Token>) -> ParseError): PatternTester<Type>
    {
        assertEquals(function(tokens).toFailure(), parser.parse(context).also { context.reset() })
        assertEquals(0, context.snapshot())
        return this
    }
}
