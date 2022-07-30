package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Expression
import com.github.derg.transpiler.ast.Mutability
import com.github.derg.transpiler.ast.Variable
import com.github.derg.transpiler.ast.Visibility
import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.lexer.Keyword
import com.github.derg.transpiler.lexer.Keyword.Type.*
import com.github.derg.transpiler.lexer.Operator
import com.github.derg.transpiler.parser.Context
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.Parser
import com.github.derg.transpiler.util.Result
import com.github.derg.transpiler.util.failureOf
import com.github.derg.transpiler.util.toSuccess
import com.github.derg.transpiler.util.valueOr

object ParserVariableDefinition : Parser<Variable>
{
    private val pattern = ParserSequence(
        ParserAllOf(
            "visibility" to ParserOptional(ParserKeyword(PUB)),
            "mutability" to ParserKeyword(VAL, VAR, MUT),
        ),
        ParserIdentifier,
        ParserOperator(Operator.Type.ASSIGN),
        ParserExpression,
    )
    
    override fun parse(context: Context): Result<Variable, ParseError>
    {
        val result = pattern.parse(context).valueOr { return failureOf(it) }
        val keywords = result[0] as Map<String, Keyword.Type?>
        val name = result[1] as Name
        val type = null
        val value = result[3] as Expression
        val visibility = visibilityOf(keywords["visibility"])
        val mutability = mutabilityOf(keywords["mutability"]!!)
        
        return Variable(name = name, type = type, value = value, visibility = visibility, mutability = mutability)
            .toSuccess()
    }
    
    private fun visibilityOf(keyword: Keyword.Type?) = when (keyword)
    {
        PUB  -> Visibility.PUBLIC
        null -> Visibility.PRIVATE
        else -> throw IllegalStateException("Illegal keyword $keyword when parsing variable visibility")
    }
    
    private fun mutabilityOf(keyword: Keyword.Type) = when (keyword)
    {
        VAL  -> Mutability.VALUE
        VAR  -> Mutability.VARYING
        MUT  -> Mutability.MUTABLE
        else -> throw IllegalStateException("Illegal keyword $keyword when parsing variable mutability")
    }
}
