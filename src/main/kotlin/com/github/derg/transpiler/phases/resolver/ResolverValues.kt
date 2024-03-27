package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * The value resolver is responsible for ensuring that values within the given [scope] can be resolved to typed values.
 * Resolution depends on the [types] present in the type table. Note that resolution cannot take place unless all types
 * that are visible from the scope have been resolved and recorded in the type table.
 *
 * All value resolution takes place before the symbol table has been constructed. This implies that the symbol table
 * cannot be used while resolving values.
 */
internal class ResolverValue(private val types: TypeTable, private val scope: Scope)
{
    /**
     * Converts the value indicated by the [node] to a typed version. This operation converts all raw names into the
     * actual id of the objects referenced where possible. If type conversion fails, an error describing the cause of
     * failure will be returned.
     */
    fun resolve(node: HirValue): Result<ThirValue, ResolveError> = when (node)
    {
        is HirAdd     -> handleInfix(Symbol.PLUS, node.lhs, node.rhs)
        is HirAnd     -> handleInfix(Symbol.AND, node.lhs, node.rhs)
        is HirBool    -> ThirBoolConst(node.value).toSuccess()
        is HirCall    -> handleCall(node)
        is HirDecimal -> TODO()
        is HirDiv     -> handleInfix(Symbol.DIVIDE, node.lhs, node.rhs)
        is HirEq      -> handleInfix(Symbol.EQUAL, node.lhs, node.rhs)
        is HirGe      -> handleInfix(Symbol.GREATER_EQUAL, node.lhs, node.rhs)
        is HirGt      -> handleInfix(Symbol.GREATER, node.lhs, node.rhs)
        is HirInteger -> handle(node)
        is HirLe      -> handleInfix(Symbol.LESS_EQUAL, node.lhs, node.rhs)
        is HirLoad    -> TODO()
        is HirLt      -> handleInfix(Symbol.LESS, node.lhs, node.rhs)
        is HirMinus   -> handlePrefix(Symbol.MINUS, node.rhs)
        is HirMod     -> handleInfix(Symbol.MODULO, node.lhs, node.rhs)
        is HirMul     -> handleInfix(Symbol.MULTIPLY, node.lhs, node.rhs)
        is HirNe      -> handleInfix(Symbol.NOT_EQUAL, node.lhs, node.rhs)
        is HirNot     -> handlePrefix(Symbol.NOT, node.rhs)
        is HirOr      -> handleInfix(Symbol.OR, node.lhs, node.rhs)
        is HirPlus    -> handlePrefix(Symbol.PLUS, node.rhs)
        is HirRead    -> TODO()
        is HirSub     -> handleInfix(Symbol.MINUS, node.lhs, node.rhs)
        is HirText    -> TODO()
        is HirXor     -> handleInfix(Symbol.XOR, node.lhs, node.rhs)
    }
    
    /**
     * Performs parameter matching and function call resolution for the [operator] acting on the [lhs] and [rhs] values.
     */
    private fun handleInfix(operator: Symbol, lhs: HirValue, rhs: HirValue): Result<ThirValue, ResolveError>
    {
        val instance = HirLoad(operator.symbol, emptyList())
        val call = HirCall(instance, listOf(null to lhs, null to rhs))
        
        return handleCall(call)
    }
    
    /**
     * Performs parameter matching and function call resolution for the [operator] acting on the [rhs] value.
     */
    private fun handlePrefix(operator: Symbol, rhs: HirValue): Result<ThirValue, ResolveError>
    {
        val instance = HirLoad(operator.symbol, emptyList())
        val call = HirCall(instance, listOf(null to rhs))
        
        return handleCall(call)
    }
    
    /**
     * Performs parameter matching between the given [type] and the user-provided [inputs]. If the function is a
     * valid candidate for being invoked with the parameters, a function call value is generated. Note that multiple
     * functions may be valid, which is the case when the function call is ambiguous.
     */
    private fun resolveCall(id: UUID, type: ThirTypeFunction, inputs: List<ThirNamedParameter>): Result<ThirCall, ResolveError>
    {
        // TODO: Support variadic arguments.
        if (type.parameters.size != inputs.size)
            return ResolveError.Placeholder.toFailure() // TODO: Replace with mismatching parameter count error.
        
        // Reject function if parameter types do not match function signature. The parameters must be ordered in the
        // same order as expected by the function before we can compare types.
        val nameToIndex = type.parameters.withIndex().associate { it.value.name to it.index }
        val sorted = inputs.withIndex().sortedBy { nameToIndex[it.value.name] ?: it.index }.map { it.value }
        
        // TODO: Handle generics, attempt to infer the type if at all possible at this point.
        if (type.parameters.zip(sorted).any { (param, input) -> param.value != input.value.value })
            return ResolveError.Placeholder.toFailure() // TODO: Replace with mismatched parameter types.
        
        // All parameters provided by the user are now confirmed to be compatible with the function under evaluation. We
        // can safely construct a function call value for this case.
        return ThirCall(
            value = type.value,
            error = type.error,
            instance = ThirLoad(type, id, emptyList()),
            parameters = sorted.map { it.value },
        ).toSuccess()
    }
    
    private fun handleCall(node: HirCall): Result<ThirValue, ResolveError>
    {
        // TODO: Support function calls that are loaded from arbitrary locations in memory.
        if (node.instance !is HirLoad)
            return ResolveError.Placeholder.toFailure()
        
        // We do not allow named arguments to appear before unnamed parameters. If any name has been specified within
        // the parameter list, then all parameters after that point must also be named.
        val firstNamed = node.parameters.withIndex().firstOrNull { it.value.first != null }
        val lastUnnamed = node.parameters.withIndex().lastOrNull { it.value.first == null }
        if (firstNamed != null && lastUnnamed != null && lastUnnamed.index > firstNamed.index)
            return ResolveError.ArgumentMisnamed(node.instance.name, node.parameters).toFailure()
        
        // We must have at least one symbol which can be considered a callable at this point. We need to examine every
        // single symbol that is within scope, as callables can come in many forms. Due to overload resolution, we need
        // to examine every candidate, to make sure there is no ambiguity or mismatches.
        val functions = scope.resolve<HirFunction>(node.instance.name)
        if (functions.isEmpty())
            return ResolveError.UnknownFunction(node.instance.name).toFailure()
        
        // TODO: This is going to be tricky... We need to filter out all functions which are definitely valid candidates
        //       due to the parameter types and names. Need to somehow match up parameters before we run through the
        //       type-checking.
        // TODO: Use the errors to report back to the user why no valid function overload was found?
        val inputs = node.parameters.mapUntilError { handle(it) }.valueOr { return it.toFailure() }
        val candidates = functions.associate { it.id to types.functions[it.id]!! }
        val (matching, errors) = candidates.map { resolveCall(it.key, it.value, inputs) }.partitionOutcomes()
        
        return when (matching.size)
        {
            1    -> matching.single().toBuiltin().toSuccess()
            0    -> ResolveError.ArgumentMismatch(node.instance.name, node.parameters).toFailure()
            else -> ResolveError.AmbiguousFunction(node.instance.name, node.parameters).toFailure()
        }
    }
    
    private fun handle(node: HirInteger): Result<ThirValue, ResolveError>
    {
        // Literals cannot be overloaded on name, as the parameter provided must be a builtin type. We do not know ahead
        // of time what the raw literal should be converted to, so we require that only a single candidate exists.
        val candidates = scope.resolve<HirLiteral>(node.literal)
        val candidate = when (candidates.size)
        {
            1    -> candidates.single()
            0    -> return ResolveError.UnknownLiteral(node.literal).toFailure()
            else -> return ResolveError.AmbiguousLiteral(node.literal, node).toFailure()
        }
        
        // We must convert the raw literal into a value which can be passed into the literal itself. The parameter must
        // be a builtin type, as only builtin types can be converted into proper typed constants. The compiler does not
        // know how to construct instances of user-defined types.
        // TODO: Replace invalid parameter error with something more appropriate.
        val literal = types.literals[candidate.id]!!
        val value = when ((literal.parameter as? ThirTypeStruct)?.symbolId)
        {
            Builtin.INT32.id -> ThirInt32Const(node.value.toInt()) // TODO: Verify that the value fits the range.
            Builtin.INT64.id -> ThirInt64Const(node.value.toLong()) // TODO: Verify that the value fits the range.
            else             -> return ResolveError.InvalidLiteralParam(node.literal).toFailure()
        }
        
        // For builtin literals, we are done - the builtin literal will always return the same value as passed in, so we
        // can return what we have. Otherwise, we need to invoke the literal with the appropriate data.
        if (candidate.id == Builtin.INT32_LIT.id || candidate.id == Builtin.INT64_LIT.id)
            return value.toSuccess()
        
        return ThirCall(
            value = literal.value,
            error = null,
            instance = ThirLoad(literal, candidate.id, emptyList()),
            parameters = listOf(value),
        ).toSuccess()
    }
    
    private fun handle(node: NamedMaybe<HirValue>): Result<ThirNamedParameter, ResolveError>
    {
        val value = resolve(node.second).valueOr { return it.toFailure() }
        
        return ThirNamedParameter(name = node.first, value = value).toSuccess()
    }
}
