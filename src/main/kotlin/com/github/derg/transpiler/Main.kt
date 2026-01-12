package com.github.derg.transpiler

import com.github.derg.transpiler.phases.converter.*
import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.parser.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.time.*

/**
 * Some sample code representing source code which is imported from some library. This source code is intended as some
 * dummy code for testing and verifying manually that running multiple files works as intended.
 */
private const val LIBRARY = """
    protected fun fibonacci(n: __builtin_i32) -> __builtin_i32
    {
        if n <= 0
            return 0
        if n == 1 || n == 2
            return 1
        return fibonacci(n - 2) + fibonacci(n - 1)
    }
"""

/**
 * Some sample code representing a binary file. This source code contains a main function which will be executed once
 * the program is running.
 */
private const val PROGRAM = """
    struct Test(val foo: __builtin_i32)
    {
        val bar: __builtin_i32 = 23
    }

    fun str(value: __builtin_i32) -> __builtin_str
    {
        var sign = ""
        var string = ""
        var current = value
        
        if current < 0
        {
            sign = "-"
            current = -current
        }
        
        while current > 0
        {
            val remainder = current % 10
            
            if remainder == 0
                string = "0" + string
            else if remainder == 1
                string = "1" + string
            else if remainder == 2
                string = "2" + string
            else if remainder == 3
                string = "3" + string
            else if remainder == 4
                string = "4" + string
            else if remainder == 5
                string = "5" + string
            else if remainder == 6
                string = "6" + string
            else if remainder == 7
                string = "7" + string
            else if remainder == 8
                string = "8" + string
            else if remainder == 9
                string = "9" + string
            
            current = current / 10
        }
        
        return sign + string
    }
    
    fun main() -> __builtin_i32
    {
        val test = Test(foo = 2)
        val data = fibonacci(test.foo + test.bar)

        __builtin_println("fibonacci(" + str(test.foo + test.bar) + ") = " + str(data))
        
        return data
    }
"""

fun main(args: Array<String>)
{
    val environment = Builtin.generateEnvironment()
    val scope = Builtin.generateScope()
    val stack = Stack()
    val evaluator = Evaluator(environment, stack)
    val resolver = Resolver(environment, scope, stack, evaluator)
    
    val compileStart = OffsetDateTime.now()
    val segments = listOf(
        parse(PROGRAM).valueOrDie(),
        parse(LIBRARY).valueOrDie(),
    )
    val module = AstModule("test", segments)
    resolver.resolve(convert(module)).valueOrDie()
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
