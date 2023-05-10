package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.Id
import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.ast.Expression
import com.github.derg.transpiler.source.ast.Segment
import com.github.derg.transpiler.source.hir.*
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
     * The type with the given [name] could not be found in the current or any outer scope.
     */
    data class UnknownType(val name: Name) : ResolveError
    
    /**
     * No function candidates were found for the function with the given [name], when invoked with the given
     * [parameters].
     */
    data class MismatchedCallableParams(val name: Name, val parameters: List<Type>) : ResolveError
    
    /**
     * During type resolution, a parameter was resolved to the [expected] type, but the default value provided for the
     * parameter resolved to the [actual] type instead.
     */
    data class MismatchedParameterType(val expected: Type, val actual: Type) : ResolveError
    
    /**
     * During type resolution, a variable was resolved to the [expected] type, but the explicitly provided type for the
     * variable resolved to the [actual] type instead.
     */
    data class MismatchedVariableType(val expected: Type, val actual: Type) : ResolveError
    
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

/**
 * Converts the given [name] into a type which has it as the name. Any other symbol with the same name, but are not a
 * type, will be ignored. If the name is not provided, [Builtin.VOID] is returned instead.
 */
internal fun SymbolTable.resolveOptionalType(name: Name?): Result<Type, ResolveError>
{
    if (name == null)
        return Builtin.VOID.toSuccess()
    
    val symbol = find(name).filterIsInstance<Type>().firstOrNull()
    return symbol?.toSuccess() ?: ResolveError.UnknownType(name).toFailure()
}

/**
 * Converts the given [expression] into a value which represents the same expression. If no expression was provided, an
 * empty value is returned instead.
 */
internal fun SymbolTable.resolveOptionalValue(expression: Expression?): Result<Value?, ResolveError> =
    if (expression == null) successOf(null) else resolveRequiredValue(expression)

/**
 * Converts the given [expression] into a value which represents the same expression.
 */
internal fun SymbolTable.resolveRequiredValue(expression: Expression): Result<Value, ResolveError> =
    ConverterExpressions(this).convert(expression)
