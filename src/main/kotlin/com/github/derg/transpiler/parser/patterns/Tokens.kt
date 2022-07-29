package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.lexer.Identifier
import com.github.derg.transpiler.lexer.Keyword
import com.github.derg.transpiler.lexer.Operator
import com.github.derg.transpiler.lexer.Structure
import com.github.derg.transpiler.parser.Context
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.Parser
import com.github.derg.transpiler.util.Result
import com.github.derg.transpiler.util.failureOf
import com.github.derg.transpiler.util.successOf

/**
 * Parses a single identifier from the context, if possible. If an identifier could be extracted from the context, the
 * current context cursor position is moved forwards to the next position.
 */
object ParserIdentifier : Parser<Name>
{
    override fun parse(context: Context): Result<Name, ParseError>
    {
        context.reset()
        val token = context.next() ?: return failureOf(ParseError.End)
        val identifier = token as? Identifier ?: return failureOf(ParseError.NotIdentifier(token))
        context.commit()
        return successOf(identifier.name)
    }
}

/**
 * Parses a single keyword from the context, if possible. Optionally, any number of keyword [types] may be specified,
 * requiring the keyword found to match one of the given types. If a keyword could be extracted from the context (and
 * matches any of the optional [types]), the current context cursor position is moved forwards to the next position.
 */
class ParserKeyword(private val types: Set<Keyword.Type> = emptySet()) : Parser<Keyword.Type>
{
    /** Helper for specifying all keyword [types] which are accepted by this parser. */
    constructor(vararg types: Keyword.Type) : this(types.toSet())
    
    override fun parse(context: Context): Result<Keyword.Type, ParseError> = TODO()
}

/**
 * Parses a single operator from the context, if possible. Optionally, any number of operator [types] may be specified,
 * requiring the operator found to match one of the given types. If an operator could be extracted from the context (and
 * matches any of the optional [types]), the current context cursor position is moved forwards to the next position.
 */
class ParserOperator(private val types: Set<Operator.Type> = emptySet()) : Parser<Operator.Type>
{
    /** Helper for specifying all operator [types] which are accepted by this parser. */
    constructor(vararg types: Operator.Type) : this(types.toSet())
    
    override fun parse(context: Context): Result<Operator.Type, ParseError>
    {
        context.reset()
        val token = context.next() ?: return failureOf(ParseError.End)
        val operator = token as? Operator ?: return failureOf(ParseError.NotOperator(token))
        if (types.isNotEmpty() && operator.type !in types)
            return failureOf(ParseError.WrongOperator(types, operator.type))
        context.commit()
        return successOf(operator.type)
    }
}

/**
 * Parses a single structural token from the context, if possible. Optionally, any number of structure [types] may be
 * specified, requiring the structure found to match one of the given types. If a structural token could be extracted
 * from the context (and matches any of the optional [types]), the current context cursor position is moved forwards to
 * the next position.
 */
class ParserStructure(private val types: Set<Structure.Type> = emptySet()) : Parser<Structure.Type>
{
    /** Helper for specifying all structure [types] which are accepted by this parser. */
    constructor(vararg types: Structure.Type) : this(types.toSet())
    
    override fun parse(context: Context): Result<Structure.Type, ParseError>
    {
        context.reset()
        val token = context.next() ?: return failureOf(ParseError.End)
        val structure = token as? Structure ?: return failureOf(ParseError.NotStructure(token))
        if (types.isNotEmpty() && structure.type !in types)
            return failureOf(ParseError.WrongStructure(types, structure.type))
        context.commit()
        return successOf(structure.type)
    }
}
