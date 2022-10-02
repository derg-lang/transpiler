package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.*
import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.lexer.*
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.ParseOk
import com.github.derg.transpiler.parser.Parser
import com.github.derg.transpiler.util.Result
import com.github.derg.transpiler.util.failureOf
import com.github.derg.transpiler.util.successOf

/**
 * Operators have a specific precedence associated with them. The higher the precedence, the later they are evaluated in
 * the expression. In other words, operators with the lowest precedence are evaluated first.
 *
 * Note that this table is not a complete set of all possible operators. Certain structural operators, such as
 * parenthesis, may be used to alter the precedence. The positioning of the operator also makes a difference - all
 * prefix operators (such as unary plus and minus) target only the expression to their immediate right, whereas postfix
 * operators target the entire expression so far to the left.
 */
private val PRECEDENCE = mapOf(
    SymbolType.AND to 4,
    SymbolType.CATCH to 7,
    SymbolType.DIVIDE to 0,
    SymbolType.EQUAL to 3,
    SymbolType.GREATER to 3,
    SymbolType.GREATER_EQUAL to 3,
    SymbolType.LESS to 3,
    SymbolType.LESS_EQUAL to 3,
    SymbolType.MINUS to 1,
    SymbolType.MODULO to 0,
    SymbolType.MULTIPLY to 0,
    SymbolType.NOT_EQUAL to 3,
    SymbolType.OR to 5,
    SymbolType.PLUS to 1,
    SymbolType.RAISE to 7,
    SymbolType.THREE_WAY to 2,
    SymbolType.XOR to 6,
)

/**
 * Joins together the [lhs] and [rhs] expression using the specified [operator].
 */
private fun mergeInfix(lhs: Expression, operator: SymbolType, rhs: Expression): Expression = when (operator)
{
    SymbolType.AND           -> Operator.And(lhs, rhs)
    SymbolType.CATCH         -> Operator.Catch(lhs, rhs)
    SymbolType.DIVIDE        -> Operator.Divide(lhs, rhs)
    SymbolType.EQUAL         -> Operator.Equal(lhs, rhs)
    SymbolType.GREATER       -> Operator.Greater(lhs, rhs)
    SymbolType.GREATER_EQUAL -> Operator.GreaterEqual(lhs, rhs)
    SymbolType.LESS          -> Operator.Less(lhs, rhs)
    SymbolType.LESS_EQUAL    -> Operator.LessEqual(lhs, rhs)
    SymbolType.MINUS         -> Operator.Subtract(lhs, rhs)
    SymbolType.MODULO        -> Operator.Modulo(lhs, rhs)
    SymbolType.MULTIPLY      -> Operator.Multiply(lhs, rhs)
    SymbolType.NOT_EQUAL     -> Operator.NotEqual(lhs, rhs)
    SymbolType.OR            -> Operator.Or(lhs, rhs)
    SymbolType.PLUS          -> Operator.Add(lhs, rhs)
    SymbolType.RAISE         -> Operator.Raise(lhs, rhs)
    SymbolType.THREE_WAY     -> Operator.ThreeWay(lhs, rhs)
    SymbolType.XOR           -> Operator.Xor(lhs, rhs)
    else                     -> throw IllegalStateException("Illegal operator $operator when parsing operator")
}

/**
 * Joins together the prefix [operator] and the [rhs] expression.
 */
private fun mergePrefix(operator: SymbolType, rhs: Expression): Expression = when (operator)
{
    SymbolType.NOT   -> Operator.Not(rhs)
    SymbolType.PLUS  -> Operator.Plus(rhs)
    SymbolType.MINUS -> Operator.Minus(rhs)
    else             -> throw IllegalStateException("Illegal operator $operator when parsing prefix operator")
}

/**
 * Generates a new fresh set of the base expression parsers. The expressions do not contain the infix operator
 * expression parser, which requires special care to properly parse. It parses itself recursively in a non-trivial
 * manner.
 */
private fun generateStandardParser(): Parser<Expression> = ParserAnyOf(
    ParserBoolExpression(),
    ParserRealExpression(),
    ParserTextExpression(),
    ParserVariableExpression(),
    ParserFunctionExpression(),
    ParserSubscriptExpression(),
    ParserParenthesisExpression(),
    ParserPrefixOperatorExpression(),
    ParserWhenExpression(),
)

/**
 * Generates a new fresh infix operator parser. The parser will only accept one of the symbols which defines one of the
 * legal infix operators.
 */
private fun generateOperatorParser(): Parser<SymbolType> = ParserSymbol(*PRECEDENCE.keys.toTypedArray())

/**
 * Parses a single expression from the provided token.
 */
class ParserExpression : Parser<Expression>
{
    private val parser = ParserRecursive { ParserOperatorExpression() }
    
    override fun produce(): Expression? = parser.produce()
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a single boolean value from the provided token.
 */
internal class ParserBoolExpression : Parser<Expression>
{
    private var expression: Expression? = null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (expression != null)
            return successOf(ParseOk.Finished)
        
        val symbol = token as? Symbol ?: return failureOf(ParseError.UnexpectedToken(token))
        expression = when (symbol.type)
        {
            SymbolType.TRUE  -> Value.Bool(true)
            SymbolType.FALSE -> Value.Bool(false)
            else             -> return failureOf(ParseError.UnexpectedToken(token))
        }
        return successOf(ParseOk.Complete)
    }
    
    override fun skipable(): Boolean = false
    override fun produce(): Expression? = expression
    override fun reset()
    {
        expression = null
    }
}

/**
 * Parses a single numeric value from the provided token.
 */
internal class ParserRealExpression : Parser<Expression>
{
    private var expression: Expression? = null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (expression != null)
            return successOf(ParseOk.Finished)
        
        val number = token as? Numeric ?: return failureOf(ParseError.UnexpectedToken(token))
        expression = Value.Real(number.value, number.type)
        return successOf(ParseOk.Complete)
    }
    
    override fun skipable(): Boolean = false
    override fun produce(): Expression? = expression
    override fun reset()
    {
        expression = null
    }
}

/**
 * Parses a single string value from the provided token.
 */
internal class ParserTextExpression : Parser<Expression>
{
    private var expression: Expression? = null
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        if (expression != null)
            return successOf(ParseOk.Finished)
        
        val string = token as? Textual ?: return failureOf(ParseError.UnexpectedToken(token))
        expression = Value.Text(string.value, string.type)
        return successOf(ParseOk.Complete)
    }
    
    override fun skipable(): Boolean = false
    override fun produce(): Expression? = expression
    override fun reset()
    {
        expression = null
    }
}

/**
 * Parses a variable access expression from the provided token.
 */
internal class ParserVariableExpression : Parser<Expression>
{
    private val parser = ParserName()
    
    override fun skipable(): Boolean = false
    override fun produce(): Expression? = parser.produce()?.let { Access.Variable(it) }
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a function call expression from the provided token.
 */
internal class ParserFunctionExpression : Parser<Expression>
{
    private val parser = ParserSequence(
        "name" to ParserName(),
        "open" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
        "params" to ParserOptional(ParserRepeating(ParserParameter(), ParserSymbol(SymbolType.COMMA))),
        "close" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
    )
    
    override fun produce(): Expression?
    {
        val values = parser.produce()
        val name = values.produce<Name>("name") ?: return null
        val params = values.produce<List<Parameter>>("params") ?: emptyList()
        return Access.Function(name, params)
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a subscript call expression from the provided token.
 */
internal class ParserSubscriptExpression : Parser<Expression>
{
    private val parser = ParserSequence(
        "name" to ParserName(),
        "open" to ParserSymbol(SymbolType.OPEN_BRACKET),
        "params" to ParserOptional(ParserRepeating(ParserParameter(), ParserSymbol(SymbolType.COMMA))),
        "close" to ParserSymbol(SymbolType.CLOSE_BRACKET),
    )
    
    override fun produce(): Expression?
    {
        val values = parser.produce()
        val name = values.produce<Name>("name") ?: return null
        val params = values.produce<List<Parameter>>("params") ?: emptyList()
        return Access.Subscript(name, params)
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a function call parameter from the provided token.
 */
private class ParserParameter : Parser<Parameter>
{
    // Note: cannot use optional parser to extract the name, as an identifier is also a legal name. This can cause the
    //       optional parser to fail on missing equals symbol, when the developer intended to pass a regular variable as
    //       a parameter to a function.
    private val parser = ParserAnyOf(
        ParserSequence("name" to ParserName(), "sym" to ParserSymbol(SymbolType.ASSIGN), "expr" to ParserExpression()),
        ParserSequence("expr" to ParserExpression()),
    )
    
    override fun produce(): Parameter?
    {
        val values = parser.produce() ?: return null
        val name = values.produce<Name>("name")
        val expression = values.produce<Expression>("expr") ?: return null
        return Parameter(name, expression)
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses an expression from in-between parenthesis from the provided token.
 */
private class ParserParenthesisExpression : Parser<Expression>
{
    private val parser = ParserSequence(
        "open" to ParserSymbol(SymbolType.OPEN_PARENTHESIS),
        "expr" to ParserRecursive { ParserOperatorExpression() },
        "close" to ParserSymbol(SymbolType.CLOSE_PARENTHESIS),
    )
    
    override fun skipable(): Boolean = false
    override fun produce(): Expression? = parser.produce().produce("expr")
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses an operator from the provided token. Prefix operators are non-trivial to construct, and involves recursively
 * parsing the stream of tokens.
 */
private class ParserPrefixOperatorExpression : Parser<Expression>
{
    private val parser = ParserSequence(
        "op" to ParserSymbol(SymbolType.PLUS, SymbolType.MINUS, SymbolType.NOT),
        "rhs" to ParserRecursive { generateStandardParser() },
    )
    
    override fun produce(): Expression?
    {
        val values = parser.produce()
        val op = values.produce<SymbolType>("op") ?: return null
        val rhs = values.produce<Expression>("rhs") ?: return null
        return mergePrefix(op, rhs)
    }
    
    override fun skipable(): Boolean = parser.skipable()
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses an operator from the provided token. Operators are non-trivial to construct, and parsing of them depends on
 * the type of operator, as well as the precedence.
 */
private class ParserOperatorExpression : Parser<Expression>
{
    private val parser = ParserSequence(
        "lhs" to generateStandardParser(),
        "terms" to ParserRepeating(ParserSequence("op" to generateOperatorParser(), "rhs" to generateStandardParser())),
    )
    
    private fun combineRecursively(lhs: Expression, terms: List<Pair<SymbolType, Expression>>, index: Int): Expression
    {
        val (op1, mhs) = terms.getOrNull(index) ?: return lhs
        val (op2, rhs) = terms.getOrNull(index + 1) ?: return mergeInfix(lhs, op1, mhs)
        
        // If next operator has higher precedence, parse left-hand of tree first
        if (PRECEDENCE[op1]!! <= PRECEDENCE[op2]!!)
            return combineRecursively(mergeInfix(lhs, op1, mhs), terms, index + 1)
        
        // Otherwise, the remainder right-hand side must be parsed recursively
        val rest = combineRecursively(rhs, terms, index + 2)
        return mergeInfix(lhs, op1, mergeInfix(mhs, op2, rest))
    }
    
    override fun produce(): Expression?
    {
        val values = parser.produce()
        val lhs = values.produce<Expression>("lhs") ?: return null
        val terms = values.produce<List<Parsers>>("terms") ?: emptyList()
        val ops = terms.produce<SymbolType>("op")
        val rhs = terms.produce<Expression>("rhs")
        return combineRecursively(lhs, ops.zip(rhs), 0)
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

private class ParserWhenExpression : Parser<Expression>
{
    private val parser = ParserSequence(
        "when" to ParserSymbol(SymbolType.WHEN),
        "expression" to ParserExpression(),
        "first" to ParserWhenBranch(),
        "remainder" to ParserRepeating(ParserWhenBranch()),
        "else" to ParserOptional(ParserSequence("else" to ParserSymbol(SymbolType.ELSE), "expr" to ParserExpression())),
    )
    
    override fun produce(): Expression?
    {
        val values = parser.produce()
        val expression = values.produce<Expression>("expression") ?: return null
        val default = values.produce<Parsers>("else")?.produce<Expression>("expr")
        val first = listOf(values.produce<Pair<Expression, Expression>>("first") ?: return null)
        val branches = values.produce<List<Pair<Expression, Expression>>>("remainder") ?: return null
        return When(expression, first + branches, default)
    }
    
    override fun skipable(): Boolean = parser.skipable()
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

private class ParserWhenBranch : Parser<Pair<Expression, Expression>>
{
    private val parser = ParserSequence(
        "condition" to ParserExpression(),
        "separator" to ParserSymbol(SymbolType.ARROW),
        "expression" to ParserExpression(),
    )
    
    override fun produce(): Pair<Expression, Expression>?
    {
        val values = parser.produce()
        val cond = values.produce<Expression>("condition") ?: return null
        val expr = values.produce<Expression>("expression") ?: return null
        return cond to expr
    }
    
    override fun skipable(): Boolean = parser.skipable()
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}
