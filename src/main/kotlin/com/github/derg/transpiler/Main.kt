package com.github.derg.transpiler

import com.github.derg.transpiler.phases.converter.*
import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.parser.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.time.*

private const val SOURCE = """
    struct Test(val foo: __builtin_i32)
    {
        val bar: __builtin_i32 = 23
    }

    fun main() -> __builtin_i32
    {
        val test = Test(foo = 2)

        __builtin_println("Hello World!")

        return fibonacci(test.foo + test.bar)
    }

    fun fibonacci(n: __builtin_i32) -> __builtin_i32
    {
        if n <= 0
            return 0
        if n == 1 || n == 2
            return 1
        return fibonacci(n - 2) + fibonacci(n - 1)
    }
"""

fun main(args: Array<String>)
{
    val environment = Builtin.generateEnvironment()
    val scope = Builtin.generateScope()
    val globals = StackFrame()
    val evaluator = Evaluator(environment, globals)
    val resolver = Resolver(environment, scope, globals, evaluator)
    
    val compileStart = OffsetDateTime.now()
    val ast = parse(SOURCE).valueOrDie()
    val hir = convert(ast)
    resolver.resolve(hir).valueOrDie()
    val compileEnd = OffsetDateTime.now()
    
    // Find the entry point into the program and load the run command.
    val main = environment.declarations.values.filterIsInstance<ThirDeclaration.Function>().single { it.name == "main" }
    val type = ThirExpression.Type(ThirType.Function(main.id, emptyList(), main.valueKind, main.errorKind))
    val call = ThirExpression.Call(type, emptyList(), main.valueKind, main.errorKind)
    
    // Analyze the outcome, lets us know what the compiler found in the source code and runtime analytics.
    val constants = environment.declarations.values.filterIsInstance<ThirDeclaration.Const>()
    val functions = environment.declarations.values.filterIsInstance<ThirDeclaration.Function>()
    val parameters = environment.declarations.values.filterIsInstance<ThirDeclaration.Parameter>()
    val structures = environment.declarations.values.filterIsInstance<ThirDeclaration.Structure>()
    val fields = environment.declarations.values.filterIsInstance<ThirDeclaration.Field>()
    
    println("")
    println("Constants: \n\t" + constants.joinToString("\n\t") { it.toString() })
    println("Functions: \n\t" + functions.joinToString("\n\t") { it.toString() })
    println("Parameters: \n\t" + parameters.joinToString("\n\t") { it.toString() })
    println("Structures: \n\t" + structures.joinToString("\n\t") { it.toString() })
    println("Fields: \n\t" + fields.joinToString("\n\t") { it.toString() })
    println("")
    println("===========================================")
    println("Finished program compilation. Running it...")
    println("===========================================")
    println("")
    
    // Run program, timing the duration of it.
    val runStart = OffsetDateTime.now()
    val outcome = evaluator.evaluate(call)
    val runEnd = OffsetDateTime.now()
    
    println("")
    println("===========================================")
    println("Outcome of program is '$outcome'")
    println("Program took ${Duration.between(compileStart, compileEnd).toNanos() / 1.0e9} seconds to compile")
    println("Program took ${Duration.between(runStart, runEnd).toNanos() / 1.0e9} seconds to run")
    println("===========================================")
    println("")
}
