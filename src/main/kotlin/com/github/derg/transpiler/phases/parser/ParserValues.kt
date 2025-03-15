package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.lexeme.*
import com.github.derg.transpiler.utils.*

/**
 * Operators have a specific [precedence] associated with them. The higher the precedence, the later they are evaluated
 * in the expression. In other words, operators with the lowest precedence are evaluated first.
 *
 * Note that this table is not a complete set of all possible operators. Certain structural operators, such as
 * parenthesis, may be used to alter the precedence. The positioning of the operator also makes a difference - all
 * prefix operators (such as unary plus and minus) target only the expression to their immediate right, whereas postfix
 * operators target the entire expression so far to the left.
 */
private enum class BinaryOperator(val apply: (AstValue, AstValue) -> AstValue, val precedence: Int)
{
    AND(::AstAnd, 4),
    ADD(::AstAdd, 1),
    DIV(::AstDivide, 0),
    EQ(::AstEqual, 3),
    GT(::AstGreater, 3),
    GE(::AstGreaterEqual, 3),
    LT(::AstLess, 3),
    LE(::AstLessEqual, 3),
    MOD(::AstModulo, 0),
    MUL(::AstMultiply, 0),
    NE(::AstNotEqual, 3),
    OR(::AstOr, 5),
    SUB(::AstSubtract, 1),
    TW(::AstThreeWay, 2),
    XOR(::AstXor, 6),
    // TODO: Please refactor these into something more sane :(
    EXCLAMATION({ lhs, rhs -> AstCatch(lhs, rhs, Capture.RAISE) }, 7),
    QUESTION({ lhs, rhs -> AstCatch(lhs, rhs, Capture.RETURN) }, 7),
}

private enum class UnaryOperator(val apply: (AstValue) -> AstValue)
{
    PLUS(::AstPlus),
    MINUS(::AstMinus),
    NOT(::AstNot),
}

/**
 * Joins together the [lhs] expression together with the remainder of the [terms], in a recursive manner.
 */
private fun mergeTerms(lhs: AstValue, terms: List<Pair<BinaryOperator, AstValue>>, index: Int = 0): AstValue
{
    val (op1, mhs) = terms.getOrNull(index) ?: return lhs
    val (op2, rhs) = terms.getOrNull(index + 1) ?: return op1.apply(lhs, mhs)
    
    // If next operator has higher precedence, parse left-hand of tree first. Otherwise, the remainder right-hand side
    // must be parsed recursively
    if (op1.precedence <= op2.precedence)
        return mergeTerms(op1.apply(lhs, mhs), terms, index + 1)
    return op1.apply(lhs, mergeTerms(op2.apply(mhs, rhs), terms, index + 2))
}

/**
 * Parses an expression from the token stream. The expressions are parsed recursively, where sub-expressions can be
 * nested within parenthesis.
 */
internal class ParserExpression : Parser<AstValue>
{
    private var inner: Parser<AstValue>? = null
    private val terms = mutableListOf<AstValue>()
    private val unary = mutableListOf<UnaryOperator>()
    private val binary = mutableListOf<BinaryOperator>()
    
    override fun reset()
    {
        inner = null
        terms.clear()
        unary.clear()
        binary.clear()
    }
    
    override fun skipable(): Boolean = false
    
    override fun produce(): AstValue
    {
        if (terms.isEmpty())
            throw IllegalStateException("Expected at least one element in expression")
        if (terms.size != binary.size + 1)
            throw IllegalStateException("Expected one less binary operator than number of terms")
        
        val lhs = terms.first()
        val terms = binary.withIndex().map { (index, op) -> op to terms[index + 1] }
        return mergeTerms(lhs, terms)
    }
    
    override fun parse(token: Token): Result<ParseOk, ParseError>
    {
        // If we are not currently preoccupied with parsing a sub-expression, we need to determine what we are parsing.
        // At this point, we must find a valid value, or something which starts off as a unary operator.
        if (inner == null) when (token)
        {
            is Identifier -> inner = loadParserOf()
            is Keyword    -> when (token.type)
            {
                Symbol.TRUE             -> inner = ParserBool()
                Symbol.FALSE            -> inner = ParserBool()
                Symbol.OPEN_PARENTHESIS -> inner = parenthesisParserOf()
                Symbol.WHEN             -> inner = whenParserOf()
                Symbol.PLUS             -> unary.add(UnaryOperator.PLUS)
                Symbol.MINUS            -> unary.add(UnaryOperator.MINUS)
                Symbol.NOT              -> unary.add(UnaryOperator.NOT)
                else                    -> return ParseError.UnexpectedToken(token).toFailure()
            }
            is Numeric    -> inner = ParserInteger()
            is Textual    -> inner = ParserText()
            else          -> return ParseError.UnexpectedToken(token).toFailure()
        }
        
        // If we have an inner parser at this point, we are working on some leaf value, or we have found a new unary
        // operator to take into consideration. If we are working with a unary operator, we must find another value to
        // work with and can bail early. Otherwise, we must invoke the inner parser to figure out what to do next.
        val inner = inner ?: return ParseOk.Incomplete.toSuccess()
        
        val outcome = inner.parse(token).valueOr { return it.toFailure() }
        if (outcome !is ParseOk.Finished)
            return outcome.toSuccess()
        
        // At this point, the inner parser is done as it encountered a token which it cannot do anything else with.
        // Here, we have either stumbled upon a binary operator, a function call, or a field access. In all cases, we
        // must consume the value and reset the inner parser for now.
        var done = false
        if (token is Keyword) when (token.type)
        {
            Symbol.PERIOD           -> this.inner = memberParserOf(inner.produce())
            Symbol.OPEN_PARENTHESIS -> this.inner = callParserOf(inner.produce())
            Symbol.AND              -> consumeSubExpression().also { binary.add(BinaryOperator.AND) }
            Symbol.DIVIDE           -> consumeSubExpression().also { binary.add(BinaryOperator.DIV) }
            Symbol.EQUAL            -> consumeSubExpression().also { binary.add(BinaryOperator.EQ) }
            Symbol.EXCLAMATION      -> consumeSubExpression().also { binary.add(BinaryOperator.EXCLAMATION) }
            Symbol.GREATER          -> consumeSubExpression().also { binary.add(BinaryOperator.GT) }
            Symbol.GREATER_EQUAL    -> consumeSubExpression().also { binary.add(BinaryOperator.GE) }
            Symbol.LESS             -> consumeSubExpression().also { binary.add(BinaryOperator.LT) }
            Symbol.LESS_EQUAL       -> consumeSubExpression().also { binary.add(BinaryOperator.LE) }
            Symbol.MINUS            -> consumeSubExpression().also { binary.add(BinaryOperator.SUB) }
            Symbol.MODULO           -> consumeSubExpression().also { binary.add(BinaryOperator.MOD) }
            Symbol.MULTIPLY         -> consumeSubExpression().also { binary.add(BinaryOperator.MUL) }
            Symbol.NOT_EQUAL        -> consumeSubExpression().also { binary.add(BinaryOperator.NE) }
            Symbol.OR               -> consumeSubExpression().also { binary.add(BinaryOperator.OR) }
            Symbol.PLUS             -> consumeSubExpression().also { binary.add(BinaryOperator.ADD) }
            Symbol.QUESTION         -> consumeSubExpression().also { binary.add(BinaryOperator.QUESTION) }
            Symbol.THREE_WAY        -> consumeSubExpression().also { binary.add(BinaryOperator.TW) }
            Symbol.XOR              -> consumeSubExpression().also { binary.add(BinaryOperator.XOR) }
            else                    -> consumeSubExpression().also { done = true }
        }
        else
            consumeSubExpression().also { done = true }
        
        // If we did not encounter more work to be done, we are finished. Otherwise, we must
        return if (done) ParseOk.Finished.toSuccess() else ParseOk.Incomplete.toSuccess()
    }
    
    /**
     * Consumes the inner parser value, with all unary operators applied to the produced value at the same time. This
     * should only be invoked once there are no more recursive components left to parse.
     */
    private fun consumeSubExpression()
    {
        var term = inner?.produce() ?: throw IllegalStateException("Expected to have an inner parser, none is active")
        inner = null
        unary.asReversed().forEach { term = it.apply(term) }
        unary.clear()
        terms.add(term)
    }
}

/**
 * Parses a variable access expression from the token stream.
 */
private fun loadParserOf(): Parser<AstLoad> =
    ParserPattern(::loadPatternOf, ::loadOutcomeOf)

private fun loadPatternOf() = ParserSequence(
    "name" to ParserIdentifier(),
    "params" to ParserOptional(ParserSequence(
        "open" to ParserSymbol(Symbol.OPEN_BRACKET),
        "params" to ParserRepeating(argumentParserOf(), ParserSymbol(Symbol.COMMA)),
        "close" to ParserSymbol(Symbol.CLOSE_BRACKET),
    ))
)

private fun loadOutcomeOf(outcome: Parsers): AstLoad =
    AstLoad(outcome["name"], outcome.get<Parsers?>("params")?.get("params") ?: emptyList())

/**
 * Parses a field access expression from the token stream.
 */
private fun memberParserOf(instance: AstValue): Parser<AstValue> =
    ParserPattern(::loadParserOf) { AstMember(instance, it) }

/**
 * Parses a function call expression from the token stream.
 */
private fun callParserOf(instance: AstValue): Parser<AstValue> =
    ParserPattern(::callPatternOf) { AstCall(instance, it["params"]) }

private fun callPatternOf() = ParserSequence(
    // NOTE: We are omitting the open parenthesis since it is consumed by the expression parser instead.
    "params" to ParserRepeating(argumentParserOf(), ParserSymbol(Symbol.COMMA)),
    "close" to ParserSymbol(Symbol.CLOSE_PARENTHESIS),
)

/**
 * Parses an expression from in-between parenthesis from the token stream.
 */
private fun parenthesisParserOf(): Parser<AstValue> =
    ParserPattern(::parenthesisPatternOf) { it["expr"] }

private fun parenthesisPatternOf() = ParserSequence(
    "open" to ParserSymbol(Symbol.OPEN_PARENTHESIS),
    "expr" to ParserExpression(),
    "close" to ParserSymbol(Symbol.CLOSE_PARENTHESIS),
)

/**
 * Parses a when expression from the token stream.
 */
private fun whenParserOf(): Parser<AstValue> =
    ParserPattern(::whenPatternOf, ::whenOutcomeOf)

private fun whenPatternOf() = ParserSequence(
    "when" to ParserSymbol(Symbol.WHEN),
    "expression" to ParserExpression(),
    "first" to whenBranchParserOf(),
    "remainder" to ParserRepeating(whenBranchParserOf()),
    "else" to ParserOptional(ParserSequence("else" to ParserSymbol(Symbol.ELSE), "expr" to ParserExpression())),
)

private fun whenOutcomeOf(values: Parsers): AstValue
{
    val expression = values.get<AstValue>("expression")
    val default = values.get<Parsers?>("else")?.get<AstValue>("expr")
    val first = listOf(values.get<Pair<AstValue, AstValue>>("first"))
    val branches = values.get<List<Pair<AstValue, AstValue>>>("remainder")
    return AstWhen(expression, first + branches, default)
}

/**
 * Parses a when expression branch from the token stream.
 */
private fun whenBranchParserOf(): Parser<Pair<AstValue, AstValue>> =
    ParserPattern(::whenBranchPatternOf, ::whenBranchOutcomeOf)

private fun whenBranchPatternOf() = ParserSequence(
    "condition" to ParserExpression(),
    "separator" to ParserSymbol(Symbol.ARROW),
    "expression" to ParserExpression(),
)

private fun whenBranchOutcomeOf(values: Parsers): Pair<AstValue, AstValue>
{
    val cond = values.get<AstValue>("condition")
    val expr = values.get<AstValue>("expression")
    return cond to expr
}
