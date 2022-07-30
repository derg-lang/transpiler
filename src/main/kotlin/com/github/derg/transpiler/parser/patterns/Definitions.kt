package com.github.derg.transpiler.parser.patterns

import com.github.derg.transpiler.ast.Variable
import com.github.derg.transpiler.parser.Context
import com.github.derg.transpiler.parser.ParseError
import com.github.derg.transpiler.parser.Parser
import com.github.derg.transpiler.util.Result

object ParserVariableDefinition : Parser<Variable>
{
    override fun parse(context: Context): Result<Variable, ParseError>
    {
        TODO("Not yet implemented")
    }
}
