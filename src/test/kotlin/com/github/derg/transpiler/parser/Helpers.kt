package com.github.derg.transpiler.parser

import com.github.derg.transpiler.ast.*
import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.lexer.Token
import com.github.derg.transpiler.lexer.tokenize
import com.github.derg.transpiler.util.successOf
import com.github.derg.transpiler.util.toFailure
import kotlin.test.assertEquals

/**
 * Converts [this] value into a literal expression if possible. The expression can only be generated from numeric,
 * boolean, and string values.
 */
fun Any.toLit(type: Name? = null): Expression = when (this)
{
    is Boolean    -> Value.Bool(this)
    is Double     -> Value.Real(toBigDecimal(), type)
    is Float      -> Value.Real(toBigDecimal(), type)
    is Int        -> Value.Real(toBigDecimal(), type)
    is String     -> Value.Text(this, type)
    is Expression -> this
    else          -> throw IllegalStateException("Cannot convert '$this' to an expression")
}

fun Name.toVar() = Access.Variable(this)
fun Name.toFun(vararg parameters: Parameter) = Access.Function(this, parameters.toList())

// Generates expressions from operations
infix fun Any.opEq(that: Any) = Operator.Equal(toLit(), that.toLit())
infix fun Any.opNe(that: Any) = Operator.NotEqual(toLit(), that.toLit())
infix fun Any.opLe(that: Any) = Operator.LessEqual(toLit(), that.toLit())
infix fun Any.opLt(that: Any) = Operator.Less(toLit(), that.toLit())
infix fun Any.opGe(that: Any) = Operator.GreaterEqual(toLit(), that.toLit())
infix fun Any.opGt(that: Any) = Operator.Greater(toLit(), that.toLit())
infix fun Any.opTw(that: Any) = Operator.ThreeWay(toLit(), that.toLit())
infix fun Any.opAnd(that: Any) = Operator.And(toLit(), that.toLit())
infix fun Any.opOr(that: Any) = Operator.Or(toLit(), that.toLit())
infix fun Any.opXor(that: Any) = Operator.Xor(toLit(), that.toLit())
infix fun Any.opAdd(that: Any) = Operator.Add(toLit(), that.toLit())
infix fun Any.opSub(that: Any) = Operator.Subtract(toLit(), that.toLit())
infix fun Any.opMul(that: Any) = Operator.Multiply(toLit(), that.toLit())
infix fun Any.opDiv(that: Any) = Operator.Divide(toLit(), that.toLit())
infix fun Any.opMod(that: Any) = Operator.Modulo(toLit(), that.toLit())
fun opNot(that: Any) = Operator.Not(that.toLit())
fun opUnPlus(that: Any) = Operator.UnaryPlus(that.toLit())
fun opUnMinus(that: Any) = Operator.UnaryMinus(that.toLit())

// Generate other useful constructs
fun Any.toPar(type: Name? = null) = Parameter(type, toLit())

/**
 * Generates variable definition from the provided input parameters.
 */
fun variableOf(
    name: Name,
    value: Any,
    type: Name? = null,
    visibility: Visibility = Visibility.PRIVATE,
    mutability: Mutability = Mutability.VALUE,
) = Variable(
    name = name,
    type = type,
    value = value.toLit(),
    visibility = visibility,
    mutability = mutability,
)

/**
 * To simplify testing of the parsing of source code for any particular pattern [factory], a helper class is provided.
 * This tester class allows the developer to write clearer and more concise test cases when parsing specific source
 * code. Each source code snippet can be tailor-made to suit certain edge-cases.
 */
class ParserTester<Type>(private val factory: () -> Parser<Type>)
{
    private lateinit var parser: Parser<Type>
    private lateinit var context: Context
    private lateinit var tokens: List<Token>
    
    /**
     * Parses the input [source] code and stores the tokens and the context for further analysis.
     */
    fun parse(source: String): ParserTester<Type>
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
    fun isGood(index: Int, value: Type): ParserTester<Type>
    {
        assertEquals(successOf(value), parser.parse(context))
        assertEquals(index, context.snapshot())
        return this
    }
    
    /**
     * Expects the parsing to fail due to the given [error]. Whenever parsing fails, the context is not permitted to
     * move forwards; its cursor must remain fixed.
     */
    fun isBad(function: (List<Token>) -> ParseError): ParserTester<Type>
    {
        assertEquals(function(tokens).toFailure(), parser.parse(context).also { context.reset() })
        assertEquals(0, context.snapshot())
        return this
    }
}
