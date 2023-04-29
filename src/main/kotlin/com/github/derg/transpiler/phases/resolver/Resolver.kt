package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.Id
import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.ast.Segment
import com.github.derg.transpiler.source.hir.Builtin
import com.github.derg.transpiler.source.hir.Module
import com.github.derg.transpiler.source.hir.Package
import com.github.derg.transpiler.source.hir.SymbolTable
import com.github.derg.transpiler.util.*

/**
 * Resolves all provided [segments] into a single package with the given [name]. The resolver phase ensures that all
 * symbols are properly linked together, converting all identifiers into an unambiguous id representing the symbol
 * itself. This phase ensures the source code is valid from a type safety perspective.
 */
fun resolve(name: Name, segments: List<Segment>): Package =
    Resolver(Builtin.SYMBOLS).resolve(name, segments).valueOr { throw IllegalStateException("Failed resolving: $it") }

/**
 * During the resolving phase, various types of errors may be encountered. These errors are typically related to type
 * errors, as type checking takes place in this phase.
 */
sealed interface ResolveError
{
    /**
     * The callable object with the given [name] is not recognized and did not resolve to anything which could be
     * invoked.
     */
    data class UnknownCallable(val name: Name) : ResolveError
    
    /**
     * The literal [name] is not recognized and cannot be used to convert the constant into a sensible value.
     */
    data class UnknownLiteral(val name: Name) : ResolveError
    
    /**
     * An unknown error, catch-all for anything that has gone wrong. Naturally, this should be replaced with more
     * specific and detailed errors.
     */
    object Unknown : ResolveError
    
    /**
     * The operation is not yet supported or just not implemented yet.
     */
    object Unsupported : ResolveError
}

/**
 *
 */
class Resolver(private val symbols: SymbolTable)
{
    /**
     *
     */
    fun resolve(name: Name, segments: List<Segment>): Result<Package, ResolveError>
    {
        // May now proceed with figuring out how to structure the package!
        val `package` = Package(Id.randomUUID(), name, SymbolTable(symbols))
        val groups = segments.groupBy { it.module }
        
        // The order in which modules are resolved depends on their dependency graph. Modules cannot be permitted to
        // have circular dependencies, as they may import symbols from each other. In order to resolve a module, all its
        // dependencies must be known ahead of time, forcing a specific evaluation order.
        // TODO: Implement me!
        // TODO: Introduce the names of all external dependencies when the time is right.
        // TODO: Should all segments be ordered in any particular order?
        
        // With all modules sorted in the appropriate order, they may be processed in that very same ordering
        for ((moduleName, groupedSegments) in groups.entries)
        {
            // TODO: Figure out a suitable default name
            val module = Module(Id.randomUUID(), moduleName ?: "__main", SymbolTable(`package`.symbols))
                .also { `package`.symbols.register(it) }
            val converter = ConverterStatements(module.symbols)
            
            // TODO: Sort all instructions in the order in which they can be initialized. How can we determine in which
            //       order all instructions should be executed? Try to execute them and just try to find an ordering
            //       that "just works"? Cannot allow side-effect functions to be invoked in that case.
            val statements = groupedSegments.flatMap { it.definitions }
            converter.prepare(statements).onFailure { return failureOf(it) }
            
            module.instructions = converter.convert(statements).valueOr { return failureOf(it) }
        }
        return `package`.toSuccess()
    }
}
