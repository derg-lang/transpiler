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
    fun foo() -> __builtin_i32
    {
        val test = 2
        var more = 3
        
        return bar(test) * more
    }
    
    fun bar(a: __builtin_i32) -> __builtin_i32
    {
        return baz(a) + 3i32
    }
    
    fun baz(a: __builtin_i32) -> __builtin_i32
    {
        return -a * a
    }
"""

private fun AstSegment.toProgram() = AstProgram(applications = listOf(this), packages = emptyList())

fun main(args: Array<String>)
{
    val ast = parse(SOURCE).valueOrDie().toProgram()
    val hir = convert(ast)
    val thir = resolve(hir).valueOrDie()
    
    check(thir).valueOrDie()
    
    val entrypoint = thir.functions.values.first { it.name == "foo" }
    val start = OffsetDateTime.now()
    val outcome = Interpreter(thir).run(entrypoint.id)
    val end = OffsetDateTime.now()
    
    println("Functions: \n\t" + thir.functions.values.joinToString("\n\t") { it.toString() })
    println("Outcome of program is '$outcome'")
    println("Program took ${Duration.between(start, end)} seconds")
}
