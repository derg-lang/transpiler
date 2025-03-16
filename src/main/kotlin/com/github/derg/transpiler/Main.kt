package com.github.derg.transpiler

import com.github.derg.transpiler.interpreter.*
import com.github.derg.transpiler.phases.converter.*
import com.github.derg.transpiler.phases.parser.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.phases.typechecker.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.utils.*
import java.time.Duration
import java.time.OffsetDateTime

private const val SOURCE = """
    struct Test[Foo, baz: __builtin_i32 = 7]
    {
        val foo: __builtin_i32 = 2
        val bar: __builtin_i32
    }
    
    fun main() -> __builtin_i32
    {
        val test = Test(bar = 23)
        
        return fibonacci(test.foo + test.bar)
    }
    
    fun fibonacci(n: __builtin_i32) -> __builtin_i32
    {
        var result = 0

        if n <= 0
            result = 0
        else if n == 1 || n == 2
            result = 1
        else
            result = fibonacci(n - 2) + fibonacci(n - 1)

        return result
    }
"""

private fun AstSegment.toProgram() = AstProgram(applications = listOf(this), packages = emptyList())

fun main(args: Array<String>)
{
    val ast = parse(SOURCE).valueOrDie().toProgram()
    val hir = convert(ast)
    val thir = resolve(hir).valueOrDie()
    
    check(thir).valueOrDie()
    
    val entrypoint = thir.functions.values.first { it.name == "main" }
    val start = OffsetDateTime.now()
    val outcome = Interpreter(thir).run(entrypoint.id)
    val end = OffsetDateTime.now()
    
    println("Functions: \n\t" + thir.functions.values.joinToString("\n\t") { it.toString() })
    println("Structures: \n\t" + thir.structs.values.joinToString("\n\t") { it.toString() })
    println("Outcome of program is '$outcome'")
    println("Program took ${Duration.between(start, end).toNanos() / 1.0e9} seconds")
}
