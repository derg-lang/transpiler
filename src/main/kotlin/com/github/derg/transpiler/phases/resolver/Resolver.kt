package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*

/**
 * Resolves all provided [segments] into a single package with the given [name]. The resolver phase ensures that all
 * symbols are properly linked together, converting all identifiers into an unambiguous id representing the symbol
 * itself. This phase ensures the source code is valid from a type safety perspective.
 */
fun resolve(name: String, segments: List<AstSegment>): ThirPackage =
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
    data class UnknownFunction(val name: String) : ResolveError
    
    /**
     * The literal [name] is not recognized and cannot be used to convert the constant into a sensible value.
     */
    data class UnknownLiteral(val name: String) : ResolveError
    
    /**
     * The type with the given [name] could not be found in the current or any outer scope.
     */
    data class UnknownType(val name: String) : ResolveError
    
    /**
     * The variable with the given [name] could not be found in the current or any outer scope.
     */
    data class UnknownVariable(val name: String) : ResolveError
    
    /**
     * During type resolution, a value was resolved to the [actual] type, but the value was required to resolve to the
     * [expected] type instead. This error happens when a function parameter is declared with a default
     */
    data class MismatchedParameterType(val expected: ThirId, val actual: ThirId) : ResolveError
    
    /**
     * During type resolution, a value was resolved to the [actual] type, but the value was required to resolve to the
     * [expected] type instead.
     */
    data class MismatchedPredicateType(val expected: ThirId, val actual: ThirId) : ResolveError
    
    /**
     * During type resolution, a return type was resolved to the [actual] type, but the return type was required to
     * resolve to the [expected] type instead.
     */
    data class MismatchedReturnType(val expected: ThirId, val actual: ThirId) : ResolveError
    
    /**
     * During type resolution, a value was resolved to the [actual] type, but the value was required to resolve to the
     * [expected] type instead.
     */
    data class MismatchedVariableType(val expected: ThirId, val actual: ThirId) : ResolveError
    
    /**
     * During type resolution, a value was resolved to the [actual] type, but the value was required to resolve to the
     * [expected] type instead.
     */
    data class MismatchedEvaluationType(val expected: ThirId, val actual: ThirId) : ResolveError
    
    /**
     * The function call to [name] with the given [arguments] resolved to multiple candidate functions, where none of
     * the candidates could be unambiguously selected.
     */
    data class ArgumentAmbiguous(val name: String, val arguments: List<ThirArgument>) : ResolveError
    
    /**
     * No function candidates were found for the function with the given [name], when invoked with the given
     * [arguments].
     */
    data class ArgumentMismatch(val name: String, val arguments: List<ThirArgument>) : ResolveError
    
    /**
     * The function with the given [name] was attempted invoked with the [arguments], where an unnamed argument was
     * provided after any named argument.
     */
    data class ArgumentMisnamed(val name: String, val arguments: List<ThirArgument>) : ResolveError
    
    /**
     * The variable must be defined with a value which does not contain any error value, but when defined its assigned
     * [value] is evaluated to an error value.
     */
    data class VariableWithError(val value: ThirValue) : ResolveError
}

/**
 * TODO: Highly in-progress and untested code, but it will do for now! Make sure to re-visit this part of the codebase
 *       at some point not too far into the future and actually clean up whatever is going on in here.
 */
class Resolver(private val symbols: ThirSymbolTable)
{
    /**
     * Constructs a package with the given [name] from the collection of [segments]. Every segment may refer to zero or
     * one module; all modules will be compiled in the appropriate order.
     */
    fun resolve(name: String, segments: List<AstSegment>): Result<ThirPackage, ResolveError>
    {
        // May now proceed with figuring out how to structure the package!
        val `package` = ThirPackage(ThirId.Static(), name, ThirSymbolTable(symbols))
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
            // TODO: Sort all instructions in the order in which they can be initialized. How can we determine in which
            //       order all instructions should be executed? Try to execute them and just try to find an ordering
            //       that "just works"? Cannot allow side-effect functions to be invoked in that case.
            // TODO: Figure out a suitable default name for modules
            val symbols = ThirSymbolTable(`package`.symbols)
            val definitions = groupedSegments.flatMap { it.definitions }
            
            ConverterDefinitions(symbols)(definitions).valueOr { return it.toFailure() }
            ThirModule(ThirId.Static(), moduleName ?: "__main", symbols).also { `package`.symbols.register(it) }
        }
        return `package`.toSuccess()
    }
}
