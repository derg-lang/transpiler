package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Mutability
import com.github.derg.transpiler.ast.Visibility
import com.github.derg.transpiler.lexer.EndOfFile
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.Tester
import com.github.derg.transpiler.parser.varOf
import org.junit.jupiter.api.Test

class TestParseVariableDefinition
{
    private val tester = Tester { ParserVariableDefinition() }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        // Mutability must be correctly parsed
        tester.parse("val foo = 0").isWip(3).isOk(1).isDone().isValue(varOf("foo", 0, mut = Mutability.VALUE))
        tester.parse("var foo = 0").isWip(3).isOk(1).isDone().isValue(varOf("foo", 0, mut = Mutability.VARYING))
        tester.parse("mut foo = 0").isWip(3).isOk(1).isDone().isValue(varOf("foo", 0, mut = Mutability.MUTABLE))
        
        // Visibility must be correctly parsed
        tester.parse("pub val foo = 0").isWip(4).isOk(1).isDone().isValue(varOf("foo", 0, vis = Visibility.PUBLIC))
        tester.parse("    val foo = 0").isWip(3).isOk(1).isDone().isValue(varOf("foo", 0, vis = Visibility.PRIVATE))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("val").isWip(1).isBad { ParseError.UnexpectedToken(EndOfFile) }
        tester.parse("val foo =").isWip(3).isBad { ParseError.UnexpectedToken(EndOfFile) }
    }
}
