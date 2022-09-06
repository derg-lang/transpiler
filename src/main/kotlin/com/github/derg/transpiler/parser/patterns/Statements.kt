package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.*
import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.lexer.SymbolType
import com.github.derg.transpiler.lexer.Token
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.ParseOk
import com.github.derg.transpiler.parser.Parser
import com.github.derg.transpiler.util.Result

/**
 * Joins together the [name] with the [operator] and the [rhs] expression.
 */
private fun merge(name: Name, operator: SymbolType, rhs: Expression): Assignment = when (operator)
{
    SymbolType.ASSIGN          -> Assignment.Assign(name, rhs)
    SymbolType.ASSIGN_PLUS     -> Assignment.AssignAdd(name, rhs)
    SymbolType.ASSIGN_MINUS    -> Assignment.AssignSubtract(name, rhs)
    SymbolType.ASSIGN_MULTIPLY -> Assignment.AssignMultiply(name, rhs)
    SymbolType.ASSIGN_MODULO   -> Assignment.AssignModulo(name, rhs)
    SymbolType.ASSIGN_DIVIDE   -> Assignment.AssignDivide(name, rhs)
    else                       -> throw IllegalStateException("Illegal operator $operator when parsing assignment")
}

/**
 * Parses a single statement from the provided token.
 */
class ParserStatement : Parser<Statement>
{
    private val parser = ParserAnyOf(
        ParserAssignment(),
        ParserBranch(),
        ParserRaise(),
    )
    
    override fun produce(): Statement? = parser.produce()
    override fun skipable(): Boolean = parser.skipable()
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a single scope from the provided token.
 */
class ParserScope : Parser<Scope>
{
    private val parser = ParserRecursive()
    {
        ParserAnyOf(
            ParserSequence("single" to ParserStatement()),
            ParserSequence(
                "open" to ParserSymbol(SymbolType.OPEN_BRACE),
                "multiple" to ParserRepeating(ParserStatement()),
                "close" to ParserSymbol(SymbolType.CLOSE_BRACE),
            )
        )
    }
    
    override fun produce(): Scope?
    {
        val values = parser.produce() ?: return null
        val statement = values.produce<Statement>("single")
        val statements = values.produce<List<Statement>>("multiple")
        val isBraced = statement == null
        return Scope(isBraced, statements ?: statement?.let { listOf(it) } ?: return null)
    }
    
    override fun skipable(): Boolean = parser.skipable()
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a single assignment from the provided token.
 */
private class ParserAssignment : Parser<Statement>
{
    private val parser = ParserSequence(
        "name" to ParserName(),
        "op" to ParserSymbol(
            SymbolType.ASSIGN,
            SymbolType.ASSIGN_PLUS,
            SymbolType.ASSIGN_MINUS,
            SymbolType.ASSIGN_MULTIPLY,
            SymbolType.ASSIGN_DIVIDE,
            SymbolType.ASSIGN_MODULO,
        ),
        "rhs" to ParserExpression(),
    )
    
    override fun produce(): Statement?
    {
        val values = parser.produce()
        val name = values.produce<Name>("name") ?: return null
        val op = values.produce<SymbolType>("op") ?: return null
        val rhs = values.produce<Expression>("rhs") ?: return null
        return merge(name, op, rhs)
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a single branch control from the provided token.
 */
private class ParserBranch : Parser<Statement>
{
    private val parser = ParserSequence(
        "if" to ParserSymbol(SymbolType.IF),
        "predicate" to ParserExpression(),
        "success" to ParserScope(),
        "other" to ParserOptional(ParserSequence("else" to ParserSymbol(SymbolType.ELSE), "failure" to ParserScope())),
    )
    
    override fun produce(): Statement?
    {
        val values = parser.produce()
        val predicate = values.produce<Expression>("predicate") ?: return null
        val success = values.produce<Scope>("success") ?: return null
        val failure = values.produce<Parsers>("other")?.produce<Scope>("failure")
        return Control.Branch(predicate, success, failure)
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}

/**
 * Parses a single raise control flow from the provided token.
 */
private class ParserRaise : Parser<Statement>
{
    private val parser = ParserSequence(
        "raise" to ParserSymbol(SymbolType.RETURN_ERROR),
        "expression" to ParserExpression(),
    )
    
    override fun produce(): Statement?
    {
        val expression = parser.produce().produce<Expression>("expression") ?: return null
        return Control.Raise(expression)
    }
    
    override fun skipable(): Boolean = false
    override fun parse(token: Token): Result<ParseOk, ParseError> = parser.parse(token)
    override fun reset() = parser.reset()
}
