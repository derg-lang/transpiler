package com.github.derg.transpiler

import com.github.derg.transpiler.phases.converter.*
import com.github.derg.transpiler.phases.parser.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.time.*

private const val SOURCE = """
    struct Test
    {
        val foo: __builtin_i32 = 2
        val bar: __builtin_i32 = 23
    }

    fun main() -> __builtin_i32
    {
        val test = Test()

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
    val compileStart = OffsetDateTime.now()
    val ast = parse(SOURCE).valueOrDie()
    val hir = convert(ast)
    val thir = resolve(hir).valueOrDie()
    val compileEnd = OffsetDateTime.now()
    
    // Find the entry point into the program and load the run command.
    val main = thir.declarations.values.last { it.name == "main" }
    val load = ThirExpression.Load(main.id, ThirType.Function(ThirType.Int32, ThirType.Void))
    val entry = ThirExpression.Call(load, emptyList(), load.valueType, load.errorType)
    
    // Analyze the outcome, lets us know what the compiler found in the source code and runtime analytics.
    val constants = thir.declarations.values.filterIsInstance<ThirDeclaration.Const>()
    val functions = thir.declarations.values.filterIsInstance<ThirDeclaration.Function>()
    val parameters = thir.declarations.values.filterIsInstance<ThirDeclaration.Parameter>()
    val structures = thir.declarations.values.filterIsInstance<ThirDeclaration.Structure>()
    val fields = thir.declarations.values.filterIsInstance<ThirDeclaration.Field>()
    
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
    val outcome = Interpreter(thir).evaluate(entry)
    val runEnd = OffsetDateTime.now()
    
    println("")
    println("Outcome of program is '$outcome'")
    println("Program took ${Duration.between(compileStart, compileEnd).toNanos() / 1.0e9} seconds to compile")
    println("Program took ${Duration.between(runStart, runEnd).toNanos() / 1.0e9} seconds to run")
    println("")
}
