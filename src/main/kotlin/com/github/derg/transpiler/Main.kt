package com.github.derg.transpiler

import com.github.derg.transpiler.phases.converter.*
import com.github.derg.transpiler.phases.parser.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.phases.typechecker.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.utils.*

private const val SOURCE = """
    fun foo() -> __builtin_i32
    {
        return bar(2) * 4
    }
    
    fun bar(a: __builtin_i32) -> __builtin_i32
    {
        return 3i32
    }
"""

private fun AstSegment.toProgram() = AstProgram(applications = listOf(this), packages = emptyList())

fun main(args: Array<String>)
{
    val ast = parse(SOURCE).valueOrDie().toProgram()
    val hir = convert(ast)
    val thir = resolve(hir).valueOrDie()
    
    check(thir).valueOrDie()
    
    println("Functions: \n\t" + thir.functions.values.joinToString("\n\t") { it.toString() })
}
