package com.github.derg.transpiler

import com.github.derg.transpiler.phases.converter.*
import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.parser.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.params.*
import org.junit.jupiter.params.provider.*
import java.io.*

class TestProgram
{
    private object ProgramList : ArgumentProvider(
        "main_returns_nothing" to null.toSuccess(),
        "main_returns_value" to 42.thir.toSuccess(),
        "main_returns_error" to 13.thir.toFailure(),
        "fibonacci" to 8.thir.toSuccess(),
        "stringify" to "1337".thir.toSuccess(),
        "aliasing" to true.thir.toSuccess(),
        "variables" to 5.thir.toSuccess(),
        "error_handling" to "failed".thir.toFailure(),
        "structures" to 5.thir.toSuccess(),
        "generics" to "true".thir.toSuccess(),
    )
    
    @ParameterizedTest
    @ArgumentsSource(ProgramList::class)
    fun `Given program, when running it, then expected value is returned`(input: Pair<String, Result<ThirExpression?, ThirExpression?>>)
    {
        val env = Builtin.generateEnvironment()
        val scope = Builtin.generateScope()
        val globals = Builtin.generateGlobals()
        val evaluator = Evaluator(env, globals)
        val resolver = Resolver(env, scope, globals, evaluator)
        
        val source = File("src/test/resources/programs/${input.first}.derg").readText()
        val segment = parse(source).valueOrDie()
        val module = AstModule("test", listOf(segment))
        resolver.resolve(convert(module)).valueOrDie()
        
        val main = env.declarations.values.last { it.name == "main" } as ThirDeclaration.Function
        
        assertEquals(input.second, evaluator.evaluate(main.thirLoad().thirCall()))
    }
}
