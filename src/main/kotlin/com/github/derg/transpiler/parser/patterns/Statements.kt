package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Assignment
import com.github.derg.transpiler.ast.Expression
import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.lexer.Operator
import com.github.derg.transpiler.parser.Context
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.Parser
import com.github.derg.transpiler.util.Result
import com.github.derg.transpiler.util.mapValue

/**
 * Parses an expression where a variable is assigned any arbitrary expression from the context, if possible. The grammar
 * does not forbid the assignment expression for occurring any location an ordinary expression can appear.
 */
object ParserAssignment : Parser<Expression>
{
    private val pattern = ParserSequence(
        ParserIdentifier,
        ParserOperator(Operator.Type.ASSIGN,
            Operator.Type.ASSIGN_PLUS,
            Operator.Type.ASSIGN_MINUS,
            Operator.Type.ASSIGN_MULTIPLY,
            Operator.Type.ASSIGN_DIVIDE,
            Operator.Type.ASSIGN_MODULO),
        ParserExpression,
    )
    
    override fun parse(context: Context): Result<Expression, ParseError>
    {
        return pattern.parse(context).mapValue { convert(it[0] as Name, it[1] as Operator.Type, it[2] as Expression) }
    }
    
    private fun convert(name: Name, operator: Operator.Type, expression: Expression): Expression = when (operator)
    {
        Operator.Type.ASSIGN          -> Assignment.Assign(name, expression)
        Operator.Type.ASSIGN_PLUS     -> Assignment.AssignAdd(name, expression)
        Operator.Type.ASSIGN_MINUS    -> Assignment.AssignSubtract(name, expression)
        Operator.Type.ASSIGN_MULTIPLY -> Assignment.AssignMultiply(name, expression)
        Operator.Type.ASSIGN_DIVIDE   -> Assignment.AssignDivide(name, expression)
        Operator.Type.ASSIGN_MODULO   -> Assignment.AssignModulo(name, expression)
        else                          -> throw IllegalStateException("Illegal operator $operator when parsing assignment")
    }
}
