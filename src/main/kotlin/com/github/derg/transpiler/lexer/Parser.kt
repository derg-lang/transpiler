package com.github.derg.transpiler.lexer

import com.github.derg.transpiler.core.Localized
import com.github.derg.transpiler.core.Node
import com.github.derg.transpiler.core.NodeExpression
import com.github.derg.transpiler.core.NodeExpression.*

/**
 *
 */
fun parse(tokens: List<Localized<Token>>): List<Node> = NodeExtractor(tokens.map { it.data }).toList()

/**
 *
 */
private class NodeExtractor(private val input: List<Token>) : Iterator<Node>, Iterable<Node>
{
    private var cursor = 0
    
    override fun iterator(): Iterator<Node> = this
    override fun hasNext(): Boolean = extractNode(input, cursor) != null
    override fun next(): Node
    {
        val (node, index) = extractNode(input, cursor)
            ?: throw IllegalStateException("No node was found in '$input' at position $cursor")
        cursor = index
        return node
    }
}

/**
 * Defines the interface which must be respected by any parser. Parsers are used to retrieve the next AST node in the
 * source code, although not all parsers are capable of understanding the source at the cursor position.
 */
private typealias NodeParser = (List<Token>, Int) -> Pair<Node, Int>?
private typealias ExpressionParser = (List<Token>, Int) -> Pair<NodeExpression, Int>?

/**
 * The collection of all parsers which are capable of extracting a node from source code. Exactly one parser must be
 * used to retrieve the next node from the source code.
 */
private val PARSERS: List<NodeParser> = listOf(
    ::extractExpressionTmp,
)

private val EXPRESSIONS: List<ExpressionParser> = listOf(
    ::extractUnary,
    ::extractNot,
    ::extractIncrementOrDecrement,
    ::extractExpression,
)

/**
 * Extracts the node at the [cursor] position from the [input] tokens.
 */
private fun extractNode(input: List<Token>, cursor: Int): Pair<Node, Int>?
{
    if (cursor >= input.size)
        return null
    
    return PARSERS.mapNotNull { it(input, cursor) }.maxByOrNull { it.second }
        ?: throw IllegalStateException("No parsers matched '$input' at position $cursor")
}

/**
 * Converts the [input] tokens into an expression if possible, starting at [cursor].
 */
private fun extractExpressionTmp(input: List<Token>, cursor: Int): Pair<NodeExpression, Int>? =
    EXPRESSIONS.mapNotNull { it(input, cursor) }.maxByOrNull { it.second }

private fun extractConstant(input: List<Token>, cursor: Int): Pair<NodeExpression, Int>?
{
    return when (val token = input[cursor])
    {
        is Numeric -> NodeExpression.Numeric(token.value)
        is Textual -> NodeExpression.Textual(token.value)
        is Keyword -> when (token.type)
        {
            Keyword.Type.TRUE  -> Bool(true)
            Keyword.Type.FALSE -> Bool(false)
            else               -> return null
        }
        else       -> return null
    } to cursor + 1
}

private fun extractIncrementOrDecrement(input: List<Token>, cursor: Int): Pair<NodeExpression, Int>?
{
    if (cursor + 1 >= input.size)
        return null
    
    val isPre = input[cursor] is Operator
    val operator = input[if (isPre) cursor else cursor + 1] as? Operator ?: return null
    val variable = input[if (isPre) cursor + 1 else cursor] as? Identifier ?: return null
    
    return when (operator.type)
    {
        Operator.Type.DECREMENT -> if (isPre) DecrementPre(variable.name) else DecrementPost(variable.name)
        Operator.Type.INCREMENT -> if (isPre) IncrementPre(variable.name) else IncrementPost(variable.name)
        else                    -> return null
    } to cursor + 2
}

private fun extractUnary(input: List<Token>, cursor: Int): Pair<NodeExpression, Int>?
{
    val token = input[cursor] as? Operator ?: return null
    if (token.type != Operator.Type.MINUS || cursor >= input.size)
        return null
    
    val (expression, index) = extractExpressionTmp(input, cursor + 1) ?: return null
    return Unary(expression) to index
}

private fun extractNot(input: List<Token>, cursor: Int): Pair<NodeExpression, Int>?
{
    val token = input[cursor] as? Operator ?: return null
    if (token.type != Operator.Type.NOT || cursor >= input.size)
        return null
    
    val (expression, index) = extractExpressionTmp(input, cursor + 1) ?: return null
    return Negate(expression) to index
}

private fun extractExpression(input: List<Token>, cursor: Int): Pair<NodeExpression, Int>?
{
    val (lhs, index) = baz(input, cursor) ?: return null
    if (index >= input.size)
        return lhs to index
    
    val op = input[index] as? Operator ?: return lhs to index
    return bar(input, index + 1, lhs, op)
}

private fun bar(input: List<Token>, cursor: Int, lhs: NodeExpression, currOp: Operator): Pair<NodeExpression, Int>?
{
    val (rhs, index) = baz(input, cursor) ?: return null
    val joined = joinExpressions(lhs, rhs, currOp) // Note: nullable, cannot return here
    
    // If no more operators exists at this point, we are done parsing infix operators
    if (index >= input.size) return joined?.let { it to index }
    val nextOp = input[index] as? Operator ?: return joined?.let { it to index }
    if (joined == null) return null
    
    // If the current operator has a higher priority than the next one, lhs + rhs is left-hand, otherwise only lhs is
    if (nextOp.type.priority <= currOp.type.priority)
        return bar(input, index + 1, joined, nextOp)
    
    val (remainder, end) = extractExpression(input, index + 1) ?: return null
    val combined = joinExpressions(rhs, remainder, nextOp) ?: return null
    return joinExpressions(lhs, combined, currOp)?.let { it to end + 1 }
}

private fun baz(input: List<Token>, cursor: Int): Pair<NodeExpression, Int>?
{
    // TODO: This does the magic thing of extracting the longest valid expression at the cursor location
    // TODO: Handle function calls, user-defined literals, parenthesis, etc.
    // TODO: Handle operators in front of expression, such as unary minus, negate, etc.
    return extractConstant(input, cursor)
}

private fun joinExpressions(lhs: NodeExpression, rhs: NodeExpression, operator: Operator): NodeExpression?
{
    return when (operator.type)
    {
        Operator.Type.PLUS          -> Plus(lhs, rhs)
        Operator.Type.MINUS         -> Minus(lhs, rhs)
        Operator.Type.MULTIPLY      -> Multiply(lhs, rhs)
        Operator.Type.DIVIDE        -> Divide(lhs, rhs)
        Operator.Type.AND           -> And(lhs, rhs)
        Operator.Type.OR            -> Or(lhs, rhs)
        Operator.Type.XOR           -> Xor(lhs, rhs)
        Operator.Type.LESS          -> Less(lhs, rhs)
        Operator.Type.LESS_EQUAL    -> LessEqual(lhs, rhs)
        Operator.Type.GREATER       -> Greater(lhs, rhs)
        Operator.Type.GREATER_EQUAL -> GreaterEqual(lhs, rhs)
        Operator.Type.EQUAL         -> Equal(lhs, rhs)
        Operator.Type.NOT_EQUAL     -> NotEqual(lhs, rhs)
        Operator.Type.THREE_WAY     -> ThreeWay(lhs, rhs)
        else                        -> null
    }
}
