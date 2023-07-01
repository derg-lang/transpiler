package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.util.*

/**
 * Resolves all provided [segments] into a single package with the given [name]. The resolver phase ensures that all
 * symbols are properly linked together, converting all identifiers into an unambiguous id representing the symbol
 * itself. This phase ensures the source code is valid from a type safety perspective.
 */
fun resolve(name: Name, segments: List<AstSegment>): ThirPackage =
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
    data class UnknownFunction(val name: Name) : ResolveError
    
    /**
     * The literal [name] is not recognized and cannot be used to convert the constant into a sensible value.
     */
    data class UnknownLiteral(val name: Name) : ResolveError
    
    /**
     * The type with the given [name] could not be found in the current or any outer scope.
     */
    data class UnknownType(val name: Name) : ResolveError
    
    /**
     * The variable with the given [name] could not be found in the current or any outer scope.
     */
    data class UnknownVariable(val name: Name) : ResolveError
    
    /**
     * No function candidates were found for the function with the given [name], when invoked with the given
     * [parameters].
     */
    data class MismatchedFunctionTypes(val name: Name, val parameters: List<ThirType>) : ResolveError
    
    /**
     * During type resolution, a parameter was resolved to the [expected] type, but the default value provided for the
     * parameter resolved to the [actual] type instead.
     */
    data class MismatchedParameterType(val expected: ThirType, val actual: ThirType) : ResolveError
    
    /**
     * During type resolution, a variable was resolved to the [expected] type, but the explicitly provided type for the
     * variable resolved to the [actual] type instead.
     */
    data class MismatchedVariableType(val expected: ThirType, val actual: ThirType) : ResolveError
    
    /**
     * The provided [type] is not permitted for usage as a predicate in a branch statement.
     */
    data class InvalidPredicateType(val type: ThirType) : ResolveError
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
    fun resolve(name: Name, segments: List<AstSegment>): Result<ThirPackage, ResolveError>
    {
        // May now proceed with figuring out how to structure the package!
        val `package` = ThirPackage(Id.randomUUID(), name, ThirSymbolTable(symbols))
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
            val module = ThirModule(Id.randomUUID(), moduleName ?: "__main").also { `package`.symbols.register(it) }
            val definitions = groupedSegments.flatMap { it.definitions }
            
            module.symbols.registerSymbols(definitions).valueOr { return failureOf(it) }
        }
        return `package`.toSuccess()
    }
}

/**
 * Converts the given [name] into a type which has it as the name. Any other symbol with the same name, but are not a
 * type, will be ignored. If the name is not provided, [Builtin.VOID] is returned instead.
 */
internal fun ThirSymbolTable.resolveOptionalType(name: Name?): Result<ThirType, ResolveError>
{
    if (name == null)
        return Builtin.VOID.toSuccess()
    
    val candidate = find(name).filterIsInstance<ThirType>().firstOrNull()
    return candidate?.toSuccess() ?: ResolveError.UnknownType(name).toFailure()
}

/**
 * Converts the given [expression] into a value which represents the same expression. If no expression was provided, an
 * empty value is returned instead.
 */
internal fun ThirSymbolTable.resolveOptionalValue(expression: AstExpression?): Result<ThirValue?, ResolveError> =
    if (expression == null) successOf(null) else resolveValue(expression)

/**
 * Converts the given [name] into a function which is compatible with the provided [parameters].
 */
internal fun ThirSymbolTable.resolveFunction(
    name: Name,
    parameters: List<ThirType>,
): Result<ThirFunction, ResolveError>
{
    // TODO: Support variable callables as well
    val functions = find(name).filterIsInstance<ThirFunction>()
    if (functions.isEmpty())
        return ResolveError.UnknownFunction(name).toFailure()
    
    val candidates = functions.filter { it.isCompatibleWith(parameters) }
    return candidates.firstOrNull()?.toSuccess() ?: ResolveError.MismatchedFunctionTypes(name, parameters).toFailure()
}

private fun ThirFunction.isCompatibleWith(parameters: List<ThirType>): Boolean
{
    if (this.params.size != parameters.size)
        return false
    return this.params.zip(parameters).all { it.first.type.id == it.second.id }
}

/**
 * Converts all the provided [statements] into instructions, baked into a new scope.
 */
internal fun ThirSymbolTable.resolveScope(statements: List<AstStatement>): Result<ThirScope, ResolveError>
{
    val inner = ThirSymbolTable(this)
    
    inner.registerSymbols(statements.filterIsInstance<AstDefinition>()).onFailure { return failureOf(it) }
    
    val instructions = statements.fold { inner.resolveStatement(it) }.valueOr { return failureOf(it) }
    return ThirScope(instructions, inner).toSuccess()
}

/**
 * Converts the given [statement] into an instruction which when executed performs the statement action.
 */
internal fun ThirSymbolTable.resolveStatement(statement: AstStatement): Result<ThirInstruction, ResolveError> =
    when (statement)
    {
        is AstAssign      -> ConverterAssign(this)(statement)
        is AstBranch      -> ConverterBranch(this)(statement)
        is AstEnter       -> TODO()
        is AstReturnError -> ConverterReturnError(this)(statement)
        is AstReturnValue -> ConverterReturnValue(this)(statement)
        is AstVariable    -> ConverterAssign(this)(AstAssign(statement.name, statement.value))
    }

/**
 * Converts the given [expression] into a value which represents the same expression.
 */
internal fun ThirSymbolTable.resolveValue(expression: AstExpression): Result<ThirValue, ResolveError> =
    when (expression)
    {
        is AstAdd          -> ConverterAdd(this)(expression)
        is AstAnd          -> ConverterAnd(this)(expression)
        is AstBool         -> ConverterBool(expression)
        is AstCall         -> ConverterCall(this)(expression)
        is AstCatch        -> TODO()
        is AstDivide       -> ConverterDivide(this)(expression)
        is AstEqual        -> ConverterEqual(this)(expression)
        is AstGreater      -> ConverterGreater(this)(expression)
        is AstGreaterEqual -> ConverterGreaterEqual(this)(expression)
        is AstLess         -> ConverterLess(this)(expression)
        is AstLessEqual    -> ConverterLessEqual(this)(expression)
        is AstMinus        -> ConverterUnaryMinus(this)(expression)
        is AstModulo       -> ConverterModulo(this)(expression)
        is AstMultiply     -> ConverterMultiply(this)(expression)
        is AstNot          -> ConverterNot(this)(expression)
        is AstNotEqual     -> ConverterNotEqual(this)(expression)
        is AstOr           -> ConverterOr(this)(expression)
        is AstPlus         -> ConverterUnaryPlus(this)(expression)
        is AstRaise        -> TODO()
        is AstReal         -> ConverterReal(this)(expression)
        is AstRead         -> ConverterRead(this)(expression)
        is AstSubscript    -> TODO()
        is AstSubtract     -> ConverterSubtract(this)(expression)
        is AstText         -> ConverterText(this)(expression)
        is AstThreeWay     -> TODO()
        is AstXor          -> ConverterXor(this)(expression)
        is AstWhen         -> TODO()
    }

/**
 * Converts the given [name] into a variable.
 */
internal fun ThirSymbolTable.resolveVariable(name: Name): Result<ThirVariable, ResolveError>
{
    // TODO: Support function variables as well
    val candidate = find(name).filterIsInstance<ThirVariable>().firstOrNull()
    return candidate?.toSuccess() ?: ResolveError.UnknownVariable(name).toFailure()
}

/**
 * Converts all the provided [definitions] into instructions, baked into a new scope.
 */
internal fun ThirSymbolTable.registerSymbols(definitions: List<AstDefinition>): Result<Unit, ResolveError>
{
    // In order to perform type checking, all statements must be declared up-front. Once the declaration has taken
    // place, we may define the symbols and use them where appropriate.
    val context = Declarator(this, IdProviderSystem)(definitions).valueOr { return failureOf(it) }
    
    // TODO: The definition order depends on how symbols depend on each other - cannot define something before all
    //       dependencies have been defined first
    context.functions.fold { DefinerFunction(this)(it.first, it.second) }.onFailure { return failureOf(it) }
    context.types.fold { DefinerType(this)(it.first, it.second) }.onFailure { return failureOf(it) }
    
    return successOf()
}
