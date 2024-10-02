package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.math.*

/**
 * Converts the input [program] into a typed variant. All symbols found within the package are stored in the symbol
 * table. All symbol references will be resolved as well, ensuring that the program is well-structured. Note that the
 * symbols are not type-checked by this point.
 */
fun resolve(program: HirProgram): Result<SymbolTable, ResolveError>
{
    val engine = ResolutionEngine()
    val outer = Builtin.GLOBAL_SCOPE
    val inner = Scope(outer)
    
    // TODO: Obviously find a better way to access the contents of the package. The engine should take care of the heavy
    //       lifting, going through the modules and segments. The order in which modules are handled does matter - we
    //       must ensure that modules which depends on others, are resolved last.
    program.applications.single().structs.forEach { inner.register(it) }
    program.applications.single().functions.forEach { inner.register(it) }
    
    // Make sure that all symbols present within the package are handled appropriately.
    engine.prepare(outer).onFailure { return it.toFailure() }
    engine.prepare(inner).onFailure { return it.toFailure() }
    
    outer.symbols.mapUntilError { engine.resolve(outer, it) }.onFailure { return it.toFailure() }
    inner.symbols.mapUntilError { engine.resolve(inner, it) }.onFailure { return it.toFailure() }
    
    return engine.symbols.toSuccess()
}

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
    data class UnknownStruct(val name: String) : ResolveError
    
    /**
     * The variable with the given [name] could not be found in the current or any outer scope.
     */
    data class UnknownVariable(val name: String) : ResolveError
    
    /**
     * The function call to [name] with the given [parameters] resolved to multiple candidate functions, where none of
     * the candidates could be unambiguously selected.
     */
    data class AmbiguousFunction(val name: String, val parameters: List<NamedMaybe<HirValue>>) : ResolveError
    
    /**
     * The function call to [name] with the given [parameter] resolved to multiple candidate literals, where none of
     * the candidates could be unambiguously selected.
     */
    data class AmbiguousLiteral(val name: String, val parameter: HirValue) : ResolveError
    
    /**
     * The usage of struct with the given [name] is ambiguous; the name resolved to multiple candidates which cannot be
     * disambiguated.
     */
    data class AmbiguousStruct(val name: String) : ResolveError
    
    /**
     * The variable assignment to [name] with the given [value] resolved to multiple candidate variables.
     */
    data class AmbiguousVariable(val name: String, val value: HirValue) : ResolveError
    
    /**
     * No function candidates were found for the function with the given [name], when invoked with the given
     * [parameters].
     */
    data class ArgumentMismatch(val name: String, val parameters: List<NamedMaybe<HirValue>>) : ResolveError
    
    /**
     * The function call was invoked in such a way, that a parameter without name was later in the list of parameters
     * than a named parameter.
     */
    data class ArgumentMisnamed(val name: String, val parameters: List<NamedMaybe<HirValue>>) : ResolveError
    
    /**
     * The definition for the variable with the given [name] resolved to no type. The variable was defined without a
     * type, and the [value] resolved to no valid type.
     */
    data class TypeMissing(val name: String, val value: HirValue) : ResolveError
    
    /**
     * The literal with the given [name] has a parameter which is not a builtin type.
     */
    data class InvalidLiteralParam(val name: String) : ResolveError
    
    /**
     * The literal with the given [value] is outside the allowed dynamic range.
     */
    data class InvalidLiteralInteger(val value: BigInteger) : ResolveError
    
    /**
     * Used to represent an error which is not yet defined.
     */
    data object Placeholder : ResolveError
}

/**
 * The resolution engine is responsible for resolving all symbols in such a manner that type-checking is performed in a
 * reliable and efficient manner. The engine determines the order in which all symbols must be resolved, providing the
 * link between symbols and the final symbol table as well.
 */
internal class ResolutionEngine
{
    val symbols = SymbolTable()
    val types = TypeTable()
    
    /**
     * Initializes the engine with the information present within the given [scope]. Symbols which are defined within
     * the scope are
     */
    fun prepare(scope: Scope): Result<Unit, ResolveError> =
        PreparerSymbol(types, scope).prepare(scope.symbols).resolve()
    
    fun resolve(scope: Scope, node: HirSymbol): Result<ThirSymbol, ResolveError> =
        ResolverSymbol(symbols, types, scope).resolve(node)
    
    fun resolve(scope: Scope, node: HirValue): Result<ThirValue, ResolveError> =
        ResolverValue(types, scope).resolve(node)
    
    fun resolve(scope: Scope, node: HirInstruction): Result<ThirInstruction, ResolveError> =
        ResolverInstruction(types, scope).resolve(node)
}

/**
 * Prepares the source code for lowering from HIR to THIR, by performing a type-collecting pass. During this stage, all
 * types are recorded and resolved, where possible.
 *
 * Note that type-checking is not performed at this phase. We only make sure that the type information is generates for
 * all symbols which need such information.
 */
private class PreparerSymbol(private val table: TypeTable, scope: Scope)
{
    private val types = ResolverType(scope)
    private val values = ResolverValue(table, scope)
    
    // Collection of symbols which remains to be processed, in the exact order they should be processed.
    private val queue = ArrayDeque<HirSymbol>()
    
    /**
     * Prepares the type resolution for the given [symbols], including all syb-symbols included as a part of it. The
     * preparation phase ensures that it is possible to look up the type of symbols, without requiring the entire symbol
     * to be fully defined.
     */
    fun prepare(symbols: List<HirSymbol>): PreparerSymbol
    {
        queue.addAll(symbols)
        return this
    }
    
    /**
     * Performs the resolution of all types, or fails trying.
     */
    fun resolve(): Result<Unit, ResolveError>
    {
        while (queue.isNotEmpty())
            handle(queue.removeFirst()).onFailure { return it.toFailure() }
        
        return Unit.toSuccess()
    }
    
    private fun handle(symbol: HirSymbol): Result<Unit, ResolveError> = when (symbol)
    {
        is HirConcept   -> TODO()
        is HirConstant  -> TODO()
        is HirField     -> handle(symbol)
        is HirFunction  -> handle(symbol)
        is HirGeneric   -> TODO()
        is HirLiteral   -> handle(symbol)
        is HirMethod    -> TODO()
        is HirParameter -> handle(symbol)
        is HirStruct    -> handle(symbol)
        is HirVariable  -> handle(symbol)
    }
    
    private fun handle(symbol: HirField): Result<Unit, ResolveError>
    {
        table.fields[symbol.id] = types.resolve(symbol.type).valueOr { return it.toFailure() }
        
        return Unit.toSuccess()
    }
    
    private fun handle(symbol: HirFunction): Result<Unit, ResolveError>
    {
        table.functions[symbol.id] = types.resolve(symbol.type).valueOr { return it.toFailure() }

//        prepare(symbol.generics)
        prepare(symbol.variables)
        prepare(symbol.parameters)
        return Unit.toSuccess()
    }
    
    private fun handle(symbol: HirLiteral): Result<Unit, ResolveError>
    {
        table.literals[symbol.id] = types.resolve(symbol.type).valueOr { return it.toFailure() }

//        prepare(symbol.variables)
        prepare(listOf(symbol.parameter))
        return Unit.toSuccess()
    }
    
    private fun handle(symbol: HirParameter): Result<Unit, ResolveError>
    {
        table.parameters[symbol.id] = types.resolve(symbol.type).valueOr { return it.toFailure() }
        
        return Unit.toSuccess()
    }
    
    private fun handle(symbol: HirStruct): Result<Unit, ResolveError>
    {
        prepare(symbol.fields)
//        prepare(symbol.methods)
//        prepare(symbol.generics)
        return Unit.toSuccess()
    }
    
    private fun handle(symbol: HirVariable): Result<Unit, ResolveError>
    {
        val type = symbol.type?.let { types.resolve(it) }?.valueOr { return it.toFailure() }
        val value = values.resolve(symbol.value).valueOr { return it.toFailure() }
        
        table.variables[symbol.id] = type ?: value.value ?: return ResolveError.TypeMissing(symbol.name, symbol.value).toFailure()
        
        return Unit.toSuccess()
    }
}
