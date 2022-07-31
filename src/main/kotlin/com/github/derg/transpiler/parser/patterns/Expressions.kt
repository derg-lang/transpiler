package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.*
import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.lexer.Keyword
import com.github.derg.transpiler.lexer.Numeric
import com.github.derg.transpiler.lexer.Operator.Type.*
import com.github.derg.transpiler.lexer.Structure
import com.github.derg.transpiler.lexer.Textual
import com.github.derg.transpiler.parser.Context
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.Parser
import com.github.derg.transpiler.util.*
import com.github.derg.transpiler.lexer.Operator.Type as OperatorType

/**
 * All expressions which do not require a highly specialized setup (such as infix operators), may be simply treated as
 * a regular pattern.
 */
private val EXPRESSIONS = arrayOf(
    ParserBoolExpression,
    ParserRealExpression,
    ParserTextExpression,
    ParserVariableExpression,
    ParserFunctionExpression,
    ParserPrefixOperatorExpression,
    ParserParenthesisExpression,
    ParserAssignExpression,
)

/**
 * Parses a single boolean value from the context, if possible. If a boolean could be extracted from the context, the
 * current context cursor position is moved forwards to the next position.
 */
object ParserBoolExpression : Parser<Expression>
{
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        context.reset()
        val token = context.next() ?: return failureOf(ParseError.End)
        val keyword = token as? Keyword ?: return failureOf(ParseError.NotExpression(token))
        val value = when (keyword.type)
        {
            Keyword.Type.TRUE  -> true
            Keyword.Type.FALSE -> false
            else               -> return failureOf(ParseError.NotExpression(token))
        }
        context.commit()
        return successOf(Value.Bool(value))
    }
}

/**
 * Parses a single numeric value from the context, if possible. If a number could be extracted from the context, the
 * current context cursor position is moved forwards to the next position.
 */
object ParserRealExpression : Parser<Expression>
{
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        context.reset()
        val token = context.next() ?: return failureOf(ParseError.End)
        val number = token as? Numeric ?: return failureOf(ParseError.NotExpression(token))
        context.commit()
        return successOf(Value.Real(number.value, number.type))
    }
}

/**
 * Parses a single text value from the context, if possible. If a string could be extracted from the context, the
 * current context cursor position is moved forwards to the next position.
 */
object ParserTextExpression : Parser<Expression>
{
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        context.reset()
        val token = context.next() ?: return failureOf(ParseError.End)
        val string = token as? Textual ?: return failureOf(ParseError.NotExpression(token))
        context.commit()
        return successOf(Value.Text(string.value, string.type))
    }
}

/**
 * Parses a variable read expression from the context, if possible. The pattern does not take into consideration any
 * additional operations which may be performed on the identifier, such as function calls or assignment.
 */
object ParserVariableExpression : Parser<Expression>
{
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        return ParserIdentifier.parse(context).mapValue { Access.Variable(it) }
    }
}

/**
 * Parses a function call expression from the context, if possible. The pattern takes into consideration all parameters
 * which are required to invoke the function. Any function call may be nested as a parameter.
 */
object ParserFunctionExpression : Parser<Expression>
{
    private val pattern = ParserSequence(
        ParserIdentifier,
        ParserStructure(Structure.Type.OPEN_PARENTHESIS),
        ParserRepeating(ParserFunctionParameter, ParserStructure(Structure.Type.COMMA)),
        ParserStructure(Structure.Type.CLOSE_PARENTHESIS),
    )
    
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        return pattern.parse(context).mapValue { Access.Function(it[0] as Name, it[2] as List<Parameter>) }
    }
}

/**
 * Parses a single optionally named parameter used in a function call from the context, if possible.
 */
private object ParserFunctionParameter : Parser<Parameter>
{
    private val pattern = ParserSequence(
        ParserOptional(ParserSequence(ParserIdentifier, ParserOperator(ASSIGN))),
        ParserExpression,
    )
    
    override fun parse(context: Context): Result<Parameter, ParseError>
    {
        return pattern.parse(context).mapValue { Parameter((it[0] as? List<*>)?.get(0) as? Name, it[1] as Expression) }
    }
}

/**
 * Parses an expressions which is surrounded by parenthesis from the context, if possible. The pattern requires that the
 * expression is fully surrounded by parenthesis, and that the expression itself is fully valid.
 */
object ParserParenthesisExpression : Parser<Expression>
{
    private val pattern = ParserSequence(
        ParserStructure(Structure.Type.OPEN_PARENTHESIS),
        ParserExpression,
        ParserStructure(Structure.Type.CLOSE_PARENTHESIS),
    )
    
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        return pattern.parse(context).mapValue { it[1] as Expression }
    }
}

/**
 * Parses an expression which starts with a valid prefix operator from the context, if possible. The pattern requires
 * that the prefix operator is permitted to operate on the expression to the right side.
 */
object ParserPrefixOperatorExpression : Parser<Expression>
{
    private val pattern = ParserSequence(
        ParserOperator(PLUS, MINUS, NOT),
        ParserExpression,
    )
    
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        return pattern.parse(context).mapValue { convert(it[0] as OperatorType, it[1] as Expression) }
    }
    
    private fun convert(operator: OperatorType, expression: Expression): Expression = when (operator)
    {
        PLUS  -> Operator.UnaryPlus(expression)
        MINUS -> Operator.UnaryMinus(expression)
        NOT   -> Operator.Not(expression)
        else  -> throw IllegalStateException("Illegal operator $operator when parsing prefix operator")
    }
}

/**
 * Parses an expression which contains one or more infix operators from the context, if possible. The pattern respects
 * operator precedence, enabling expressions such as `1 * 2 + 3 * 4` to correctly evaluate to `14`.
 */
object ParserInfixOperatorExpression : Parser<Expression>
{
    /**
     * The precedence of operators determine the execution order. The operators are executed in order from the lowest
     * value to the highest value, i.e. operators with a precedence of 0 are executed first. If two operators have the
     * same precedence, the evaluation order is left-to-right, where `expr op expr op expr` is evaluated as
     * `(expr op expr) op expr`.
     */
    private val precedences = mapOf(
        AND to 4,
        DIVIDE to 0,
        EQUAL to 3,
        GREATER to 3,
        GREATER_EQUAL to 3,
        LESS to 3,
        LESS_EQUAL to 3,
        MINUS to 1,
        MODULO to 0,
        MULTIPLY to 0,
        NOT_EQUAL to 3,
        OR to 5,
        PLUS to 1,
        THREE_WAY to 2,
        XOR to 6,
    )
    
    private val expressions = ParserAnyOf(*EXPRESSIONS)
    private val operators = ParserOperator(precedences.keys)
    
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        val snapshot = context.snapshot()
        val lhs = expressions.parse(context).valueOr { return failureOf(it) }
        val op1 = operators.parse(context).onFailure { context.revert(snapshot) }.valueOr { return failureOf(it) }
        return parse(context, lhs, op1).onFailure { context.revert(snapshot) }
    }
    
    private fun parse(context: Context, lhs: Expression, op1: OperatorType): Result<Expression, ParseError>
    {
        val mhs = expressions.parse(context).valueOr { return failureOf(it) }
        val op2 =
            operators.parse(context).onFailure { context.reset() }.valueOr { return successOf(join(lhs, mhs, op1)) }
        
        if (precedences.getValue(op1) <= precedences.getValue(op2))
            return parse(context, join(lhs, mhs, op1), op2)
        val rhs = ParserExpression.parse(context).valueOr { return failureOf(it) }
        return join(lhs, join(mhs, rhs, op2), op1).toSuccess()
    }
    
    private fun join(lhs: Expression, rhs: Expression, operator: OperatorType): Expression = when (operator)
    {
        AND           -> Operator.And(lhs, rhs)
        DIVIDE        -> Operator.Divide(lhs, rhs)
        EQUAL         -> Operator.Equal(lhs, rhs)
        GREATER       -> Operator.Greater(lhs, rhs)
        GREATER_EQUAL -> Operator.GreaterEqual(lhs, rhs)
        LESS          -> Operator.Less(lhs, rhs)
        LESS_EQUAL    -> Operator.LessEqual(lhs, rhs)
        MINUS         -> Operator.Subtract(lhs, rhs)
        MODULO        -> Operator.Modulo(lhs, rhs)
        MULTIPLY      -> Operator.Multiply(lhs, rhs)
        NOT_EQUAL     -> Operator.NotEqual(lhs, rhs)
        OR            -> Operator.Or(lhs, rhs)
        PLUS          -> Operator.Add(lhs, rhs)
        THREE_WAY     -> Operator.ThreeWay(lhs, rhs)
        XOR           -> Operator.Xor(lhs, rhs)
        else          -> throw IllegalStateException("Illegal operator $operator when parsing prefix operator")
    }
}

/**
 * Parses an expression where a variable is assigned any arbitrary expression from the context, if possible. The grammar
 * does not forbid the assignment expression for occurring any location an ordinary expression can appear.
 */
object ParserAssignExpression : Parser<Expression>
{
    private val pattern = ParserSequence(
        ParserIdentifier,
        ParserOperator(ASSIGN, ASSIGN_PLUS, ASSIGN_MINUS, ASSIGN_MULTIPLY, ASSIGN_DIVIDE, ASSIGN_MODULO),
        ParserExpression,
    )
    
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        return pattern.parse(context).mapValue { convert(it[0] as Name, it[1] as OperatorType, it[2] as Expression) }
    }
    
    private fun convert(name: Name, operator: OperatorType, expression: Expression): Expression = when (operator)
    {
        ASSIGN          -> Assignment.Assign(name, expression)
        ASSIGN_PLUS     -> Assignment.AssignAdd(name, expression)
        ASSIGN_MINUS    -> Assignment.AssignSubtract(name, expression)
        ASSIGN_MULTIPLY -> Assignment.AssignMultiply(name, expression)
        ASSIGN_DIVIDE   -> Assignment.AssignDivide(name, expression)
        ASSIGN_MODULO   -> Assignment.AssignModulo(name, expression)
        else            -> throw IllegalStateException("Illegal operator $operator when parsing assignment")
    }
}

/**
 * Parses an expression from the context, if possible. The expression which is extracted may be of any type, length, or
 * any arbitrary complexity. The extracted expression will be the largest node in the abstract syntax tree possible to
 * extract from the current context's cursor location.
 */
object ParserExpression : Parser<Expression>
{
    private val pattern = ParserAnyOf(ParserInfixOperatorExpression, *EXPRESSIONS)
    
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        return pattern.parse(context)
    }
}
