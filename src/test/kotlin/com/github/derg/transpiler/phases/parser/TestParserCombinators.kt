package com.github.derg.transpiler.phases.parser

import com.github.derg.transpiler.source.ast.Expression
import com.github.derg.transpiler.source.ast.Operator
import com.github.derg.transpiler.source.ast.Value
import com.github.derg.transpiler.source.lexeme.EndOfFile
import com.github.derg.transpiler.source.lexeme.Numeric
import com.github.derg.transpiler.source.lexeme.SymbolType
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

private const val REAL = "real"
private const val BOOL = "bool"
private const val TEXT = "text"

class TestParserAnyOf
{
    @Test
    fun `Given valid token, when parsing empty, then finished`()
    {
        val tester = Tester { ParserAnyOf<Unit>() }
        
        tester.parse("").isDone()
    }
    
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserAnyOf(ParserRealExpression(), ParserBoolExpression()) }
        
        tester.parse("42").isOk(1).isDone().isValue(42.toExp()).resets()
        tester.parse("true").isOk(1).isDone().isValue(true.toExp()).resets()
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserAnyOf(
                ParserSequence(REAL to ParserRealExpression(), BOOL to ParserBoolExpression()),
                ParserSequence(TEXT to ParserTextExpression(), BOOL to ParserBoolExpression()),
                ParserSequence(TEXT to ParserTextExpression()),
            )
        }
        
        tester.parse("42 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 42.toExp()).hasValue(BOOL, true.toExp()).resets()
        tester.parse("\"foo\" false").isOk(2).isDone()
            .hasValue(TEXT, "foo".toExp()).hasValue(BOOL, false.toExp()).resets()
        tester.parse("\"foo\"").isOk(1).isDone()
            .hasValue(TEXT, "foo".toExp()).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserAnyOf(ParserRealExpression(), ParserBoolExpression()) }
        
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
    }
}

class TestParserAllOf
{
    @Test
    fun `Given valid token, when parsing empty, then finished`()
    {
        val tester = Tester { ParserAllOf() }
        
        tester.parse("").isDone()
    }
    
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserAllOf(REAL to ParserRealExpression(), BOOL to ParserBoolExpression()) }
        
        tester.parse("42 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 42.toExp()).hasValue(BOOL, true.toExp())
        tester.parse("true 42").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 42.toExp()).hasValue(BOOL, true.toExp())
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserAllOf(
                REAL to ParserSequence("a" to ParserRealExpression(), "b" to ParserRealExpression()),
                BOOL to ParserBoolExpression(),
                TEXT to ParserOptional(ParserTextExpression()),
            )
        }
        
        tester.parse("1 2 true").isWip(2).isOk(1).isDone()
            .hasValue(REAL, "a" to 1.toExp(), "b" to 2.toExp())
            .hasValue(BOOL, true.toExp())
        tester.parse("\"foo\" 1 2 true").isWip(3).isOk(1).isDone()
            .hasValue(REAL, "a" to 1.toExp(), "b" to 2.toExp())
            .hasValue(BOOL, true.toExp())
            .hasValue(TEXT, "foo".toExp())
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserAllOf(REAL to ParserRealExpression(), BOOL to ParserBoolExpression()) }
        
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
        tester.parse("42").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("true").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserSequence
{
    @Test
    fun `Given valid token, when parsing empty, then finished`()
    {
        val tester = Tester { ParserSequence() }
        
        tester.parse("").isDone()
    }
    
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester()
        {
            ParserSequence(
                REAL to ParserRealExpression(),
                BOOL to ParserBoolExpression(),
                TEXT to ParserTextExpression(),
            )
        }
        
        tester.parse("1 true \"foo\"").isWip(2).isOk(1).isDone()
            .hasValue(REAL, 1.toExp()).hasValue(BOOL, true.toExp()).hasValue(TEXT, "foo".toExp())
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserSequence(
                REAL to ParserSequence("a" to ParserRealExpression(), "b" to ParserRealExpression()),
                BOOL to ParserSequence("a" to ParserBoolExpression(), "b" to ParserBoolExpression()),
                TEXT to ParserOptional(ParserTextExpression()),
            )
        }
        
        tester.parse("1 2 true false").isWip(3).isOk(1).isDone()
            .hasValue(REAL, "a" to 1.toExp(), "b" to 2.toExp())
            .hasValue(BOOL, "a" to true.toExp(), "b" to false.toExp())
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester { ParserSequence(REAL to ParserRealExpression(), BOOL to ParserBoolExpression()) }
        
        tester.parse("bah").isBad { ParseError.UnexpectedToken(it[0]) }
        tester.parse("1").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}

class TestParserRepeating
{
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserRepeating(ParserRealExpression(), ParserSymbol(SymbolType.COMMA)) }
        
        tester.parse("").isDone().isValue(emptyList())
        tester.parse("1").isOk(1).isDone().isValue(listOf(1.toExp())).resets(emptyList())
        tester.parse("1,").isOk(2).isDone().isValue(listOf(1.toExp())).resets(emptyList())
        tester.parse("1,2").isOk(3).isDone().isValue(listOf(1.toExp(), 2.toExp())).resets(emptyList())
        tester.parse("1,2,").isOk(4).isDone().isValue(listOf(1.toExp(), 2.toExp())).resets(emptyList())
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester = Tester()
        {
            ParserRepeating(
                ParserSequence(REAL to ParserRealExpression(), BOOL to ParserBoolExpression()),
                ParserSymbol(SymbolType.COMMA),
            )
        }
        
        tester.parse("1 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 1.toExp()).hasValue(BOOL, true.toExp())
        tester.parse("1 true,").isWip(1).isOk(2).isDone()
            .hasValue(REAL, 1.toExp()).hasValue(BOOL, true.toExp())
        tester.parse("1 true, 2 false").isWip(1).isOk(2).isWip(1).isOk(1).isDone()
            .hasValue(REAL, 1.toExp(), 2.toExp()).hasValue(BOOL, true.toExp(), false.toExp())
        tester.parse("1 true, 2 false,").isWip(1).isOk(2).isWip(1).isOk(2).isDone()
            .hasValue(REAL, 1.toExp(), 2.toExp()).hasValue(BOOL, true.toExp(), false.toExp())
    }
    
    @Test
    fun `Given valid token, when parsing empty separator, then value produced`()
    {
        val tester =
            Tester { ParserRepeating(ParserSequence(REAL to ParserRealExpression(), BOOL to ParserBoolExpression())) }
        
        tester.parse("1 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 1.toExp())
        tester.parse("1 true 2 false").isWip(1).isOk(1).isWip(1).isOk(1).isDone()
            .hasValue(REAL, 1.toExp(), 2.toExp()).hasValue(BOOL, true.toExp(), false.toExp())
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester = Tester()
        {
            ParserRepeating(
                ParserSequence(REAL to ParserRealExpression(), BOOL to ParserBoolExpression()),
                ParserSymbol(SymbolType.COMMA),
            )
        }
        
        tester.parse("1").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("1 true, 2").isWip(1).isOk(2).isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
    
    @Test
    fun `Given produced values, when resetting, then values are retained`()
    {
        val parser = ParserRepeating(ParserRealExpression())
            .also { it.parse(Numeric(1, null)) }
            .also { it.parse(EndOfFile) }
        val items = parser.produce()
        
        assertTrue(items.isNotEmpty())
        parser.reset()
        assertTrue(items.isNotEmpty())
    }
}

class TestParserOptional
{
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserOptional(ParserRealExpression()) }
        
        tester.parse("").isDone().isValue(null).resets()
        tester.parse("42").isOk(1).isDone().isValue(42.toExp()).resets()
    }
    
    @Test
    fun `Given valid token, when parsing complex, then value produced`()
    {
        val tester =
            Tester { ParserOptional(ParserSequence(REAL to ParserRealExpression(), BOOL to ParserBoolExpression())) }
        
        tester.parse("").isDone()
            .hasValue(REAL, null).hasValue(BOOL, null)
        tester.parse("42 true").isWip(1).isOk(1).isDone()
            .hasValue(REAL, 42.toExp()).hasValue(BOOL, true.toExp())
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        val tester =
            Tester { ParserOptional(ParserSequence(REAL to ParserRealExpression(), BOOL to ParserBoolExpression())) }
        
        tester.parse("1 bah").isWip(1).isBad { ParseError.UnexpectedToken(it[1]) }
    }
}

class TestParserPattern
{
    private val pattern = ParserSequence(
        "lhs" to ParserRealExpression(),
        "rhs" to ParserRealExpression(),
    )
    
    private fun converter(outcome: Parsers): Expression?
    {
        val lhs = outcome.produce<Value.Real>("lhs") ?: return null
        val rhs = outcome.produce<Value.Real>("rhs") ?: return null
        return Operator.Add(lhs, rhs)
    }
    
    @Test
    fun `Given valid token, when parsing simple, then value produced`()
    {
        val tester = Tester { ParserPattern(pattern, ::converter) }
        
        tester.parse("1 2").isWip(1).isOk(1).isDone().isValue(1 opAdd 2).resets()
    }
    
    @Test
    fun `Given invalid token, when parsing simple, then correct error`()
    {
        val tester = Tester { ParserPattern(pattern, ::converter) }
        
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("1").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("1 true").isWip(1).isBad { ParseError.UnexpectedToken(it[1]) }
    }
}
