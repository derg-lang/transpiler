package com.github.derg.transpiler.parser

import com.github.derg.transpiler.lexer.Keyword
import com.github.derg.transpiler.lexer.Operator
import com.github.derg.transpiler.lexer.Structure
import com.github.derg.transpiler.lexer.Token
import com.github.derg.transpiler.util.Result

/**
 * All data which is read from the source code which is parsed must be converted into individual nodes. All nodes
 * together form the abstract syntax tree describing the full source code. The abstract syntax tree may be utilized to
 * perform various operations on the source code in a manner which is simpler than performing text manipulations.
 */
interface Parser<Type>
{
    /**
     * Extracts a single value from the [context] if possible. Any reads from the context which are successful will
     * force the context's current cursor location to move forwards. Since the context may be in a dirty state before
     * reading the next token, any reads of tokens *must* invoke [Context.reset] first.
     *
     * Once a result has been successfully extracted from the context after invoking [Context.next], the context state
     * must be persisted by invoking [Context.commit]. This ensures the data read from the context will not be lost
     * when the context is reset before later reads.
     */
    fun parse(context: Context): Result<Type, ParseError>
}

/**
 * Whenever source code is parsed into an abstract syntax tree, various error conditions prevent further processing from
 * taking place. This is always the case if the source code ends abruptly or otherwise does not respect the grammar of
 * the programming language.
 */
sealed class ParseError
{
    // Structural issues related to the parsing
    object End : ParseError()
    
    // The data acquired from the context has the wrong format
    data class NotKeyword(val token: Token) : ParseError()
    data class NotOperator(val token: Token) : ParseError()
    data class NotStructure(val token: Token) : ParseError()
    data class NotIdentifier(val token: Token) : ParseError()
    data class NotExpression(val token: Token) : ParseError()
    
    // The data acquired from the context has the wrong value
    data class WrongKeyword(val expected: Set<Keyword.Type>, val actual: Keyword.Type) : ParseError()
    data class WrongOperator(val expected: Set<Operator.Type>, val actual: Operator.Type) : ParseError()
    data class WrongStructure(val expected: Set<Structure.Type>, val actual: Structure.Type) : ParseError()
}
