package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Mutability
import com.github.derg.transpiler.ast.Visibility
import com.github.derg.transpiler.parser.ParseError.End
import com.github.derg.transpiler.parser.ParseError.NotExpression
import com.github.derg.transpiler.parser.ParserTester
import com.github.derg.transpiler.parser.variableOf
import org.junit.jupiter.api.Test

class TestParseVariableDefinition
{
    private val tester = ParserTester { ParserVariableDefinition }
    
    @Test
    fun `Given valid token, when parsing, then correctly parsed`()
    {
        // Mutability must be correctly parsed
        tester.parse("val foo = 0").isGood(4, variableOf("foo", 0, mutability = Mutability.VALUE))
        tester.parse("var foo = 0").isGood(4, variableOf("foo", 0, mutability = Mutability.VARYING))
        tester.parse("mut foo = 0").isGood(4, variableOf("foo", 0, mutability = Mutability.MUTABLE))
        
        // Visibility must be correctly parsed
        tester.parse("pub val foo = 0").isGood(5, variableOf("foo", 0, visibility = Visibility.PUBLIC))
        tester.parse("    val foo = 0").isGood(4, variableOf("foo", 0, visibility = Visibility.PRIVATE))
    }
    
    @Test
    fun `Given invalid token, when parsing, then correct error`()
    {
        tester.parse("").isBad { End }
        tester.parse("val").isBad { End }
        tester.parse("val foo =").isBad { End }
        tester.parse("val foo = if").isBad { NotExpression(it[3]) }
    }
}
