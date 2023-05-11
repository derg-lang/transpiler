package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Definition
import com.github.derg.transpiler.source.ast.Statement
import com.github.derg.transpiler.source.hir.Function
import com.github.derg.transpiler.source.hir.SymbolTable
import com.github.derg.transpiler.source.hir.Type
import com.github.derg.transpiler.util.Result
import com.github.derg.transpiler.util.failureOf
import com.github.derg.transpiler.util.successOf
import com.github.derg.transpiler.util.valueOr

/**
 * Defines the function based on the contents provided by the function definition. Once defined, the function will be
 * ready for usage in all other use-cases, such as optimization and code generation.
 */
internal class DefinerFunction(private val symbols: SymbolTable)
{
    operator fun invoke(function: Function, statements: List<Statement>): Result<Unit, ResolveError>
    {
        function.scope = symbols.resolveScope(statements).valueOr { return failureOf(it) }
        function.params.forEach { function.scope.symbols.register(it) }
        return successOf()
    }
}

/**
 *
 */
internal class DefinerType(private val symbols: SymbolTable)
{
    operator fun invoke(type: Type, definition: Definition.Type): Result<Unit, ResolveError>
    {
        // TODO: Implement me
        return successOf()
    }
}
