package com.github.derg.transpiler.parser

import com.github.derg.transpiler.ast.*
import com.github.derg.transpiler.ast.Function
import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.lexer.EndOfFile
import com.github.derg.transpiler.lexer.Token
import com.github.derg.transpiler.lexer.tokenize
import com.github.derg.transpiler.parser.patterns.Parsers
import com.github.derg.transpiler.parser.patterns.produce
import com.github.derg.transpiler.util.failureOf
import com.github.derg.transpiler.util.isSuccess
import com.github.derg.transpiler.util.successOf
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue

/**
 * Converts [this] value into a literal expression if possible. The expression can only be generated from numeric,
 * boolean, and string values.
 */
fun Any.toExp(type: Name? = null): Expression = when (this)
{
    is Boolean    -> Value.Bool(this)
    is Double     -> Value.Real(toBigDecimal(), type)
    is Float      -> Value.Real(toBigDecimal(), type)
    is Int        -> Value.Real(toBigDecimal(), type)
    is String     -> Value.Text(this, type)
    is Expression -> this
    else          -> throw IllegalStateException("Cannot convert '$this' to an expression")
}

// Various primitive conversion functions
fun Name.toVar() = Access.Variable(this)
fun Name.toFun(vararg parameters: Parameter) = Access.Function(this, parameters.toList())
fun Name.toSub(vararg parameters: Parameter) = Access.Subscript(this, parameters.toList())
fun Any.toPar(name: Name? = null) = Parameter(name, toExp())

// Generates expressions from operations
fun opNot(that: Any) = Operator.Not(that.toExp())
fun opPlus(that: Any) = Operator.Plus(that.toExp())
fun opMinus(that: Any) = Operator.Minus(that.toExp())
infix fun Any.opEq(that: Any) = Operator.Equal(toExp(), that.toExp())
infix fun Any.opNe(that: Any) = Operator.NotEqual(toExp(), that.toExp())
infix fun Any.opLe(that: Any) = Operator.LessEqual(toExp(), that.toExp())
infix fun Any.opLt(that: Any) = Operator.Less(toExp(), that.toExp())
infix fun Any.opGe(that: Any) = Operator.GreaterEqual(toExp(), that.toExp())
infix fun Any.opGt(that: Any) = Operator.Greater(toExp(), that.toExp())
infix fun Any.opTw(that: Any) = Operator.ThreeWay(toExp(), that.toExp())
infix fun Any.opAnd(that: Any) = Operator.And(toExp(), that.toExp())
infix fun Any.opOr(that: Any) = Operator.Or(toExp(), that.toExp())
infix fun Any.opXor(that: Any) = Operator.Xor(toExp(), that.toExp())
infix fun Any.opAdd(that: Any) = Operator.Add(toExp(), that.toExp())
infix fun Any.opSub(that: Any) = Operator.Subtract(toExp(), that.toExp())
infix fun Any.opMul(that: Any) = Operator.Multiply(toExp(), that.toExp())
infix fun Any.opDiv(that: Any) = Operator.Divide(toExp(), that.toExp())
infix fun Any.opMod(that: Any) = Operator.Modulo(toExp(), that.toExp())
infix fun Any.opCatch(that: Any) = Operator.Catch(toExp(), that.toExp())
infix fun Any.opRaise(that: Any) = Operator.Raise(toExp(), that.toExp())

// Generates assignment from operations
infix fun Name.assign(that: Any) = Assignment.Assign(this, that.toExp())
infix fun Name.assignAdd(that: Any) = Assignment.AssignAdd(this, that.toExp())
infix fun Name.assignSub(that: Any) = Assignment.AssignSubtract(this, that.toExp())
infix fun Name.assignMul(that: Any) = Assignment.AssignMultiply(this, that.toExp())
infix fun Name.assignMod(that: Any) = Assignment.AssignModulo(this, that.toExp())
infix fun Name.assignDiv(that: Any) = Assignment.AssignDivide(this, that.toExp())

// Generates statements
fun callOf(expression: Any) = Control.Call(expression.toExp())
fun raiseOf(expression: Any) = Control.Raise(expression.toExp())
fun returnOf(expression: Any? = null) = Control.Return(expression?.toExp())

/**
 * Generates variable definition from the provided input parameters.
 */
fun varOf(
    name: Name,
    value: Any,
    type: Name? = null,
    vis: Visibility = Visibility.PRIVATE,
    mut: Mutability = Mutability.VALUE,
) = Variable(
    name = name,
    type = type,
    value = value.toExp(),
    visibility = vis,
    mutability = mut,
)

/**
 * Generates function definition from the provided input parameters.
 */
fun funOf(
    name: Name,
    valType: Name? = null,
    errType: Name? = null,
    vis: Visibility = Visibility.PRIVATE,
    params: List<FunctionParameter> = emptyList(),
    scope: Scope = scopeOf(true),
) = Function(
    name = name,
    valueType = valType,
    errorType = errType,
    parameters = params,
    visibility = vis,
    scope = scope,
)

/**
 * Generates function parameter definition from the provided input parameters.
 */
fun parOf(
    name: Name,
    type: Name? = null,
    value: Any? = null,
    mut: Mutability = Mutability.VALUE,
) = FunctionParameter(
    name = name,
    type = type,
    value = value?.toExp(),
    mutability = mut,
)

/**
 * Generates a branch structure from the provided input parameters.
 */
fun branchOf(predicate: Any, success: Scope, failure: Scope? = null) =
    Control.Branch(predicate.toExp(), success, failure)

/**
 * Generates a scope definition from the provided input parameters.
 */
fun scopeOf(isBraced: Boolean, vararg statements: Statement) =
    Scope(isBraced, statements.toList())

/**
 * To simplify testing of the parsing of source code for any particular pattern [factory], a helper class is provided.
 * This tester class allows the developer to write clearer and more concise test cases when parsing specific source
 * code. Each source code snippet can be tailor-made to suit certain edge-cases.
 */
class Tester<Type>(private val factory: () -> Parser<Type>)
{
    internal lateinit var parser: Parser<Type>
    private lateinit var tokens: List<Token>
    private var cursor: Int = 0
    
    /**
     * Parses the input [source] code and stores the tokens and the context for further analysis.
     */
    fun parse(source: String): Tester<Type>
    {
        parser = factory()
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
     * The parser must be reverted to its original state once reset - it must produce the [value] upon resetting.
     */
    fun resets(value: Type? = null): Tester<Type>
    {
        parser.reset()
        assertEquals(value, parser.produce())
        return this
    }
}

/**
 * The parser is expected to have produced the given [value] already and stored it under the given [key].
 */
fun <Type, Bundle : Parsers?> Tester<Bundle>.hasValue(key: String, value: Type): Tester<Bundle>
{
    assertEquals(value, parser.produce()?.produce(key))
    return this
}

/**
 * The parser is expected to have produced the given [values] already and stored it under the given [key]. The parser
 * must be a deeply nested object - that is, a [Parsers] bundle containing other [Parsers].
 */
fun <Type, Bundle : Parsers?> Tester<Bundle>.hasValue(key: String, vararg values: Pair<String, Type>): Tester<Bundle>
{
    val parsers = parser.produce()?.produce<Parsers>(key)
    assertEquals(values.map { it.second }, values.mapNotNull { parsers?.produce(it.first) })
    return this
}

/**
 * The parser is expected to have produced the given [values] already and stored them under the given [key].
 */
fun <Type> Tester<List<Parsers>>.hasValue(key: String, vararg values: Type): Tester<List<Parsers>>
{
    assertEquals(values.toList(), parser.produce()?.produce<Type>(key))
    return this
}
