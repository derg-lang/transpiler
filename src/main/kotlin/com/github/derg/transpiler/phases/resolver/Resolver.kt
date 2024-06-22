package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.math.*

/**
 * Converts everything present within the [symbol] into a type table describing all types found within it.
 */
fun resolve(symbol: HirFunction): Result<Tables, ResolveError>
{
    val tables = Tables(TypeTable(), SymbolTable())
    val resolver = Resolver(tables.types, tables.symbols, Builtin.GLOBAL_SCOPE)

//    resolver.resolve(Builtin.GLOBAL_SCOPE, symbol).onFailure { return it.toFailure() }
    
    return tables.toSuccess()
}

/**
 * During the resolving phase, various types of errors may be encountered. These errors are typically related to type
 * errors, as type checking takes place in this phase.
 */
sealed interface ResolveError
{
    /**
     * The function call to [name] with the given [parameter] resolved to multiple candidate literals, where none of
     * the candidates could be unambiguously selected.
     */
    data class AmbiguousLiteral(val name: String, val parameter: HirValue) : ResolveError
    
    /**
     * The usage of the type with the given [name] is ambiguous; the name resolved to multiple candidates which cannot
     * be disambiguated.
     */
    data class AmbiguousType(val name: String) : ResolveError
    
    /**
     * The literal with the given [value] is outside the allowed range of the literal.
     */
    data class InvalidInteger(val value: BigInteger) : ResolveError
    
    /**
     * The [instance] has been evaluated to something which is not callable, when the current situation expected a
     * callable value.
     */
    data class InvalidCall(val instance: HirInstance) : ResolveError
    
    /**
     * The parameter [type] is not permitted to be used as a parameter in the literal [name].
     */
    data class InvalidType(val name: String, val type: HirType) : ResolveError
    
    /**
     * The literal [name] is not recognized and cannot be used to convert the constant into a sensible value.
     */
    data class UnknownLiteral(val name: String) : ResolveError
    
    /**
     * The type with the given [name] could not be found in the current or any outer scope.
     */
    data class UnknownType(val name: String) : ResolveError
}

/**
 * The resolver ensures that all symbols, types, instructions, and values can all be converted into the appropriate
 * typed variant. All type-checking is performed at this stage, if at all possible. Note that during this process, not
 * all symbols and/or types are fully defined.
 *
 * @param types The table holding the type of symbols which will be granted a type during resolution.
 * @param symbols The table of all symbols resolved during the resolution phase.
 * @param scope The base scope in which all resolution should take place.
 */
internal class Resolver(private val types: TypeTable, private val symbols: SymbolTable, private val scope: Scope)
{
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Metadata
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    fun resolve(input: HirTemplate): Result<ThirTemplate, ResolveError> = when (input)
    {
        is HirTemplate.Type  -> ThirTemplate.Type.toSuccess()
        is HirTemplate.Value -> resolve(input)
    }
    
    fun resolve(input: HirTemplate.Value): Result<ThirTemplate.Value, ResolveError> =
        resolve(input.type).mapValue { ThirTemplate.Value(it) }
    
    fun resolve(input: HirType): Result<ThirType, ResolveError> = when (input)
    {
        is HirType.Call -> resolve(input)
        is HirType.Data -> resolve(input)
    }
    
    fun resolve(input: HirType.Call): Result<ThirType.Call, ResolveError>
    {
        val valueType = input.valueType?.let { resolve(it) }?.valueOr { return it.toFailure() }
        val errorType = input.errorType?.let { resolve(it) }?.valueOr { return it.toFailure() }
        val parameters = input.parameters.mapUntilError { resolve(it) }.valueOr { return it.toFailure() }
        
        return ThirType.Call(
            valueType = valueType,
            errorType = errorType,
            parameters = parameters,
        ).toSuccess()
    }
    
    fun resolve(input: HirType.Data): Result<ThirType.Data, ResolveError>
    {
        // TODO: Support aliasing and unions.
        val candidates = scope.resolve(input.name).filterIsInstance<HirStruct>()
        val candidate = when (candidates.size)
        {
            1    -> candidates.single()
            0    -> return ResolveError.UnknownType(input.name).toFailure()
            else -> return ResolveError.AmbiguousType(input.name).toFailure()
        }
        
        return ThirType.Data(
            symbolId = candidate.id,
            mutability = input.mutability,
            generics = emptyList(),
        ).toSuccess()
    }
    
    private fun resolve(input: HirType.Parameter): Result<ThirType.Parameter, ResolveError>
    {
        val type = resolve(input.type).valueOr { return it.toFailure() }
        
        return ThirType.Parameter(
            name = input.name,
            type = type,
        ).toSuccess()
    }
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Symbols
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    fun resolve(input: HirStruct): Result<ThirStruct, ResolveError>
    {
        return ThirStruct(
            id = input.id,
            name = input.name,
            visibility = input.visibility,
            fieldIds = input.fields.map { it.id }.toSet(),
            methodIds = input.methods.map { it.id }.toSet(),
            genericIds = input.generics.map { it.id }.toSet(),
        ).toSuccess()
    }
    
    fun resolve(input: HirField): Result<ThirField, ResolveError>
    {
        val type = resolve(input.type).valueOr { return it.toFailure() }
        val value = input.value?.let { resolve(it) }?.valueOr { return it.toFailure() }
        
        return ThirField(
            id = input.id,
            name = input.name,
            type = type,
            value = value,
            visibility = input.visibility,
            assignability = input.assignability,
        ).toSuccess()
    }
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Values
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    private fun HirInstance.Value.resolveCall(parameters: List<NamedMaybe<HirValue>>): Result<ThirValue, ResolveError>
    {
        // Example code which we are currently trying to resolve:
        // `my_function()(1, 2, 3)`
        
        // The actual callable is hidden inside some expression, we must ensure the expression is legal and cannot raise
        // any errors.
        val callable = resolve(value).valueOr { return it.toFailure() }
        if (callable.errorType != null)
            return ResolveError.InvalidCall(this).toFailure()
        
        // In order to invoke the callable, it must actually be callable. All callables has a signature which describe
        // what it can accept, and what it returns.
        val signature = callable.valueType as? ThirType.Call ?: return ResolveError.InvalidCall(this).toFailure()
        
        // Once we have the signature of the callable, we can attempt to resolve the actual call with the provided
        // arguments. If the signature is a match with the provided arguments, the call can be accepted. All parameters
        // are provided in arbitrary order by the caller, so we need to re-organize all parameters such that they are in
        // the same order as expected by the callable instance.
        val unsortedParameters = parameters.mapUntilError { resolve(it) }.valueOr { return it.toFailure() }
        // TODO: Order all parameters according to the names in the signature!
        // TODO: Ensure that the parameter lists actually are compatible with each other!
        
        return ThirCall(
            instance = ThirInstance.Value(callable),
            parameters = unsortedParameters.map { it.second },
            valueType = signature.valueType,
            errorType = signature.errorType,
        ).toSuccess()
    }
    
    private fun HirInstance.Named.resolveCall(parameters: List<NamedMaybe<HirValue>>): Result<ThirValue, ResolveError>
    {
        // Example code which we are currently trying to resolve:
        // `my_function[foo, bar, baz](1, 2, 3)`
        
        // All inputs should be converted into the typed form ahead of time, to avoid a potential massive amount of
        // conversion.
        val unsortedParameters = parameters.mapUntilError { resolve(it) }.valueOr { return it.toFailure() }
        
        // Knowing the actual inputs we are working with, all symbols which match the name might be valid candidates. We
        // need to filter away all candidates which do not resolve to a valid call invocation.
        val (candidates, failures) = scope.resolve(name)
            .mapNotInnerNull { it.resolveCall(generics, unsortedParameters) }
            .partitionOutcomes()
        
        return when (candidates.size)
        {
            1    -> candidates.single().toSuccess()
            0    -> TODO()
            else -> TODO()
        }
    }
    
    private fun HirSymbol.resolveCall(generics: List<NamedMaybe<HirValue>>, parameters: List<NamedMaybe<ThirValue>>): Result<ThirCall?, ResolveError> =
        when (this)
        {
            // TODO: Support variables, parameters, and any other similar sort of symbol if they are callable.
            is HirFunction -> resolveCall(generics, parameters)
            else           -> null.toSuccess()
        }
    
    private fun HirFunction.resolveCall(generics: List<NamedMaybe<HirValue>>, parameters: List<NamedMaybe<ThirValue>>): Result<ThirCall?, ResolveError>
    {
        // We receive something along the lines of `some_function[foo, bar, baz](1, 2, 3)`, which we need to translate
        // into something we can work with. The function name and templates (`some_function[foo, bar, baz]`) together
        // form the instance, and these determine what the parameters actually should be interpreted as.
        //
        // The template arguments may be given in any arbitrary order, so we need to ensure they are sorted according to
        // the ordering expected by the symbol. The same holds for the arguments, they are given in arbitrary order.
        //
        // Once we know the ordering of all templates and arguments, we can build up the callable signature. Since we
        // know the templates at this point, we can substitute all template references with the actual type which should
        // be used. In doing so, the signature will hold no reference to the templates, and instead form a specific
        // variant of the call site. This specific variant can be used later to monomorphism purposes later on.
        
        // NOTE: We assume that the templates and arguments are in the correct order, and that they are compatible. This
        //       is not true, but it makes it easier to start working on this code!
        // TODO: Order all parameters according to the names in the signature!
        // TODO: Ensure that the parameter lists actually are compatible with each other!
        // TODO: Massively improve the way that generics actually should work here!
        // TODO: Some form of parameter conversion interplay with templates also needs to be implemented here.
        
        val foo: List<ThirInstance.Generic> = this.generics.zip(generics).map()
        { (generic, input) ->
            when (generic.template)
            {
                is HirTemplate.Type  ->
                {
                    val load = input.second as? HirLoad ?: TODO()
                    val named = load.instance as? HirInstance.Named ?: TODO()
                    val candidate = scope.resolve(named.name).singleOrNull() ?: TODO() // Structs, unions, aliases, etc.
                    
                    // We have some sort of recursive hogwash happening here. Need to determine the candidate based on
                    // the generic parameters in `named`. We cannot accept the first choice we pick up here.
                    ThirInstance.Generic.Type(ThirType.Data(symbolId = candidate.id, mutability = Mutability.IMMUTABLE, generics = emptyList()))
                    
                    TODO()
                    // ThirInstance.Generic.Type(((input.second as HirLoad).instance as HirInstance.Named).name)
                }
                is HirTemplate.Value -> resolve(input.second).mapValue { ThirInstance.Generic.Value(it) }.valueOr { return it.toFailure() }
            }
        }
        
        // All parameters provided by the user are now confirmed to be compatible with the function under evaluation. We
        // can safely construct a function call value for this case.
        return ThirCall(
            instance = ThirInstance.Named(id, foo),
            parameters = parameters.map { it.second },
            valueType = valueType?.let { resolve(it) }?.valueOr { return it.toFailure() },
            errorType = errorType?.let { resolve(it) }?.valueOr { return it.toFailure() },
        ).toSuccess()
    }
    
    fun resolve(input: HirValue): Result<ThirValue, ResolveError> = when (input)
    {
        is HirAdd     -> TODO()
        is HirAnd     -> TODO()
        is HirBool    -> ThirConstBool(input.value).toSuccess()
        is HirCall    -> resolve(input)
        is HirCatch   -> TODO()
        is HirDecimal -> TODO()
        is HirDiv     -> TODO()
        is HirEq      -> TODO()
        is HirGe      -> TODO()
        is HirGt      -> TODO()
        is HirInteger -> resolve(input)
        is HirLe      -> TODO()
        is HirLoad    -> TODO()
        is HirLt      -> TODO()
        is HirMinus   -> TODO()
        is HirMod     -> TODO()
        is HirMul     -> TODO()
        is HirNe      -> TODO()
        is HirNot     -> TODO()
        is HirOr      -> TODO()
        is HirPlus    -> TODO()
        is HirSub     -> TODO()
        is HirText    -> TODO()
        is HirXor     -> TODO()
    }
    
    fun resolve(input: HirCall): Result<ThirValue, ResolveError> = when (input.instance)
    {
        is HirInstance.Named -> input.instance.resolveCall(input.parameters)
        is HirInstance.Value -> input.instance.resolveCall(input.parameters)
    }
    
    fun resolve(input: HirInteger): Result<ThirValue, ResolveError>
    {
        // Literals cannot be overloaded on name, as the parameter provided must be a builtin type. We do not know ahead
        // of time what the raw literal should be converted to, so we require that only a single candidate exists.
        val candidates = scope.resolve(input.literal).filterIsInstance<HirLiteral>()
        val candidate = when (candidates.size)
        {
            1    -> candidates.single()
            0    -> return ResolveError.UnknownLiteral(input.literal).toFailure()
            else -> return ResolveError.AmbiguousLiteral(input.literal, input).toFailure()
        }
        
        // We must convert the raw literal into a value which can be passed into the literal itself. The parameter must
        // be a builtin integer, as only builtin types can be converted into proper typed constants. The compiler does
        // not know how to construct instances of user-defined types.
        val inputType = resolve(candidate.parameter.type).valueOr { return it.toFailure() }
        if (inputType !is ThirType.Data)
            return ResolveError.InvalidType(input.literal, candidate.parameter.type).toFailure()
        
        val value = when (inputType.symbolId)
        {
            Builtin.INT32.id -> input.value.toInt32().valueOr { return it.toFailure() }
            Builtin.INT64.id -> input.value.toInt64().valueOr { return it.toFailure() }
            else             -> return ResolveError.InvalidType(input.literal, candidate.parameter.type).toFailure()
        }
        
        // For builtin literals, we are done - the builtin literal will always return the same value as passed in, so we
        // can return what we have.
        if (candidate.id == Builtin.INT32_LIT.id || candidate.id == Builtin.INT64_LIT.id)
            return value.toSuccess()
        
        // For non-builtin literals, we need to invoke the appropriate function representing the literal.
        return ThirCall(
            valueType = resolve(candidate.valueType).valueOr { return it.toFailure() },
            errorType = null,
            instance = ThirInstance.Named(candidate.id, emptyList()),
            parameters = listOf(value),
        ).toSuccess()
    }
    
    private fun resolve(input: NamedMaybe<HirValue>): Result<NamedMaybe<ThirValue>, ResolveError> =
        resolve(input.second).mapValue { NamedMaybe(input.first, it) }
}
