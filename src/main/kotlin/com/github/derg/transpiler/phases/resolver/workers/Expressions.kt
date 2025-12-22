package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.math.*

val INT32_MIN = Int.MIN_VALUE.toBigInteger()
val INT32_MAX = Int.MAX_VALUE.toBigInteger()
val INT64_MIN = Long.MIN_VALUE.toBigInteger()
val INT64_MAX = Long.MAX_VALUE.toBigInteger()
val FLOAT32_MIN = Float.MIN_VALUE.toBigDecimal()
val FLOAT32_MAX = Float.MAX_VALUE.toBigDecimal()
val FLOAT64_MIN = Double.MIN_VALUE.toBigDecimal()
val FLOAT64_MAX = Double.MAX_VALUE.toBigDecimal()

/**
 * Constructs a worker which is capable of resolving the [node] to a proper THIR expression. The worker will operate in
 * the given [scope], using the information provided through the given [env].
 *
 * If the worker is provided a [kindHint], it will perform two-way type inference. Note that merely providing a type
 * hint does not enforce type safety. The implementation does not verify that the type provided from the user matches
 * the type of the resolved expression.
 *
 * Note that the expression worker can halt until all requirements are defined, provided [requireDefinition] is set. In
 * this case, the worker will halt once an undefined symbol is encountered. This mode must be enabled when the
 * expression is to be evaluated during a compilation phase.
 *
 * @param kindHint A hint indicating which type the expression should take after resolution.
 * @param requireDefinition Whether the expression require all symbol dependencies to be defined before resolution.
 */
fun expressionDefinerOf(
    evaluator: Evaluator,
    node: HirExpression,
    env: Environment,
    scope: Scope,
    kindHint: ThirKind?,
    requireDefinition: Boolean,
): Worker<ThirExpression> = when (node)
{
    is HirExpression.Bool       -> BoolDefiner(node)
    is HirExpression.Call       -> CallDefiner(evaluator, node, env, scope, requireDefinition)
    is HirExpression.Catch      -> CatchDefiner(evaluator, node, env, scope, requireDefinition)
    is HirExpression.Decimal    -> DecimalDefiner(node)
    is HirExpression.Field      -> FieldAccessDefiner(evaluator, node, env, scope, kindHint, requireDefinition)
    is HirExpression.Identifier -> IdentifierDefiner(node, env, scope, kindHint, requireDefinition)
    is HirExpression.Integer    -> IntegerDefiner(node)
    is HirExpression.Text       -> StringDefiner(node)
}

/**
 * Converts from a HIR bool type to a THIR bool type.
 */
internal class BoolDefiner(private val node: HirExpression.Bool) : Worker<ThirExpression>
{
    override fun process(): Result<ThirExpression.Bool, Outcome>
    {
        return ThirExpression.Bool(node.value).toSuccess()
    }
}

/**
 * Converts from a HIR integer type to a THIR integer type.
 */
internal class IntegerDefiner(private val node: HirExpression.Integer) : Worker<ThirExpression>
{
    override fun process(): Result<ThirExpression, Outcome> = when (node.literal)
    {
        INT32_LIT_NAME -> node.value.asInt32()
        INT64_LIT_NAME -> node.value.asInt64()
        // TODO: Remove this else clause by fully defining in the language all possible literals.
        else           -> throw IllegalArgumentException("Unexpected integer literal '${node.literal}'")
    }
    
    private fun BigInteger.asInt32(): Result<ThirExpression, Outcome>
    {
        if (this < INT32_MIN || this > INT32_MAX)
            return Outcome.InvalidInteger(this).toFailure()
        return ThirExpression.Int32(toInt()).toSuccess()
    }
    
    private fun BigInteger.asInt64(): Result<ThirExpression, Outcome>
    {
        if (this < INT64_MIN || this > INT64_MAX)
            return Outcome.InvalidInteger(this).toFailure()
        return ThirExpression.Int64(toLong()).toSuccess()
    }
}

/**
 * Converts from a HIR decimal type to a THIR decimal type.
 */
internal class DecimalDefiner(private val node: HirExpression.Decimal) : Worker<ThirExpression>
{
    override fun process(): Result<ThirExpression, Outcome> = when (node.literal)
    {
        FLOAT32_LIT_NAME -> node.value.asFloat32()
        FLOAT64_LIT_NAME -> node.value.asFloat64()
        // TODO: Remove this else clause by fully defining in the language all possible literals.
        else             -> throw IllegalArgumentException("Unexpected floating point literal '${node.literal}'")
    }
    
    private fun BigDecimal.asFloat32(): Result<ThirExpression, Outcome>
    {
        // TODO: Verify that the input number is within legal limits.
        return ThirExpression.Float32(toFloat()).toSuccess()
    }
    
    private fun BigDecimal.asFloat64(): Result<ThirExpression, Outcome>
    {
        // TODO: Verify that the input number is within legal limits.
        return ThirExpression.Float64(toDouble()).toSuccess()
    }
}

/**
 * Converts from a HIR string type to a THIR string type.
 */
internal class StringDefiner(private val node: HirExpression.Text) : Worker<ThirExpression>
{
    override fun process(): Result<ThirExpression, Outcome> = when (node.literal)
    {
        // TODO: Verify that the input string is within legal limits.
        STR_LIT_NAME -> ThirExpression.Str(node.value).toSuccess()
        // TODO: Remove this else clause by fully defining in the language all possible literals.
        else         -> throw IllegalArgumentException("Unexpected string literal '${node.literal}'")
    }
}

/**
 *
 */
internal class FieldAccessDefiner(
    evaluator: Evaluator,
    private val node: HirExpression.Field,
    private val env: Environment,
    scope: Scope,
    kindHint: ThirKind?,
    requireDefinition: Boolean,
) : Worker<ThirExpression>
{
    val worker = expressionDefinerOf(evaluator, node.instance, env, scope, null, requireDefinition)
    
    var instance: ThirExpression? = null
    
    override fun process(): Result<ThirExpression, Outcome>
    {
        if (instance == null)
            instance = worker.process().valueOr { return it.toFailure() }
        
        val kind = instance!!.valueKind as? ThirKind.Value
            ?: return Outcome.Unhandled("Kind of $node is not a value kind").toFailure()
        val type = kind.type as? ThirType.Structure
            ?: return Outcome.InvalidStructure(kind.type).toFailure()
        val symbol = env.declarations[type.structureId] as? ThirDeclaration.Structure
            ?: return Outcome.RequireDeclaration(setOf(type.structureId)).toFailure()
        val field = symbol.fieldIds
            .mapNotNull { env.declarations[it] as? ThirDeclaration.Field }
            .singleOrNull { it.name == node.identifier.name }
            ?: return Outcome.Unhandled("No fields with name '${node.identifier.name}' found").toFailure()
        
        return ThirExpression.Field(
            instance = instance!!,
            fieldId = field.id,
            valueKind = field.kind,
        ).toSuccess()
    }
}

/**
 * Converts from a HIR memory access to a THIR memory access. This operation depends on which type of symbol the node
 * refers to. In order to create a THIR representation of the load, the symbol must be declared, although it does not
 * need to be defined.
 *
 * If the identifier worker should attempt to perform a two-way type inference, a [kindHint] must be provided. Once the
 * hint is given, the type hint will be used to filter away all possible candidates whose types do not resolve to the
 * hint.
 *
 * When the expression is intended for executing during the compilation phase, it may be marked as [requireDefinition].
 * In this situation, whenever the expression encounters a symbol which is not defined, the worker will pause until the
 * offending identifier has been defined.
 */
internal class IdentifierDefiner(
    private val node: HirExpression.Identifier,
    private val env: Environment,
    private val scope: Scope,
    private val kindHint: ThirKind?,
    private val requireDefinition: Boolean,
) : Worker<ThirExpression>
{
    override fun process(): Result<ThirExpression, Outcome>
    {
        // TODO: When type parameters are provided on a standalone identifier, we need to verify the provided type
        //       parameters against the symbol declaration. All candidates which are not compatible with the type
        //       parameters must be filtered away.
        if (node.typeParameters.isNotEmpty())
            return Outcome.Unsupported("Type parameters on standalone identifiers are not supported yet").toFailure()
        
        // We need to figure out which candidate we are to work on. In the initial solution here, we ignore most of the
        // difficult parts, such as determining whether the candidate's type matches the type hint, generics, and all
        // those pesky parts. We assume that function passing does not happen yet, but as soon as it is permitted we
        // need to consider function overloading as well when selecting our candidate.
        val candidates = scope.find(node.name)
        val candidate = when (candidates.size)
        {
            1    -> candidates.single()
            0    -> return Outcome.UnknownIdentifier(node.name).toFailure()
            else -> return Outcome.OverloadedIdentifier(node.name).toFailure()
        }
        
        // Another complication for the moment, is that we do not know the type of our candidate. We look through all
        // declarations we know of, and if we find it in there we are basically done. Otherwise, we must mandate that
        // the candidate be declared before we can continue.
        val symbol = env.declarations[candidate]
            ?: return Outcome.RequireDeclaration(setOf(candidate)).toFailure()
        
        val def: Any? = when (symbol)
        {
            is ThirDeclaration.Const         -> symbol.def
            is ThirDeclaration.Field         -> symbol.def
            is ThirDeclaration.Function      -> symbol.def
            is ThirDeclaration.Parameter     -> symbol.def
            is ThirDeclaration.Structure     -> symbol.def
            is ThirDeclaration.TypeParameter -> symbol.def
            is ThirDeclaration.Variable      -> null
        }
        if (requireDefinition && def == null)
            return Outcome.RequireDefinition(setOf(symbol.id)).toFailure()
        
        // TODO: Somehow handle the type parameters here as well. This is where the magic happens, after all!
        val type = when (symbol)
        {
            is ThirDeclaration.Const         -> symbol.kind
            is ThirDeclaration.Field         -> symbol.kind
            is ThirDeclaration.Function      -> ThirKind.Value(ThirType.Function(symbol.id, emptyList(), symbol.valueKind, symbol.errorKind))
            is ThirDeclaration.Parameter     -> symbol.kind
            is ThirDeclaration.Structure     -> ThirKind.Value(ThirType.Structure(symbol.id, emptyList()))
            is ThirDeclaration.TypeParameter -> symbol.kind
            is ThirDeclaration.Variable      -> symbol.kind
        }
        return ThirExpression.Load(candidate, type).toSuccess()
    }
}

/**
 * Converts from a HIR function call to a THIR function call. This operation highly depends on the environment and all
 * auxiliary information available in the compiler. In order to perform a call resolution, all candidates must be
 * declared, although they do not need to be defined. The candidate parameters must all be defined when default
 * parameter values are involved, however.
 *
 * When the expression is intended for executing during the compilation phase, it may be marked as [requireDefinition].
 * In this situation, whenever the expression encounters a symbol which is not defined, the worker will pause until the
 * offending symbol has been defined.
 */
internal class CallDefiner(
    private val evaluator: Evaluator,
    private val node: HirExpression.Call,
    private val env: Environment,
    private val scope: Scope,
    private val requireDefinition: Boolean,
) : Worker<ThirExpression>
{
    override fun process(): Result<ThirExpression, Outcome>
    {
        // TODO: We need to be able to work with function signatures provided from cases when the function is passed as
        //       a parameter, not just when we can read it directly from the environment.
        if (node.instance !is HirExpression.Identifier)
            return Outcome.Unsupported("Functions as r-values are not supported yet").toFailure()
        
        // In order to resolve a function call, we need to find all possible overloads which are involved. Since we do
        // not know which candidate we will select ahead of time, we require that all candidates are declared before we
        // do any work.
        val symbols = scope.find(node.instance.name)
        val undeclaredSymbols = symbols.filter { it !in env.declarations }
        if (undeclaredSymbols.isNotEmpty())
            return Outcome.RequireDeclaration(undeclaredSymbols.toSet()).toFailure()
        
        // Once we have the collection of symbols which may be callable, we need to narrow this selection down to
        // exactly one candidate.
        val candidates = symbols.mapNotNull { env.declarations[it] }
        if (candidates.isEmpty())
            return Outcome.UnknownIdentifier(node.instance.name).toFailure()
        
        val (overloads, errors) = candidates.map { resolve(it, node.instance) }.partitionOutcomes()
        val undeclaredParams = errors.filterIsInstance<Outcome.RequireDeclaration>().flatMap { it.ids }
        if (undeclaredParams.isNotEmpty())
            return Outcome.RequireDeclaration(undeclaredParams.toSet()).toFailure()
        val undefinedParams = errors.filterIsInstance<Outcome.RequireDefinition>().flatMap { it.ids }
        if (undefinedParams.isNotEmpty())
            return Outcome.RequireDefinition(undefinedParams.toSet()).toFailure()
        
        // TODO: If the overload has not yet been defined, we need to halt analysis if `requireDefinition` is true.
        return when (overloads.size)
        {
            1    -> overloads.single().toSuccess()
            0    -> Outcome.NoOverloadAvailable(node.instance.name, errors).toFailure()
            else -> Outcome.OverloadedIdentifier(node.instance.name).toFailure()
        }
    }
    
    /**
     * Attempt to resolve the HIR expression with respect to the given [symbol]. We cannot resolve the expression unless
     * we know which target we are applying it to - that is, to support use-friendly two-way type inference, we need to
     * know what types the callable expects.
     */
    private fun resolve(symbol: ThirDeclaration, instance: HirExpression.Identifier): Result<ThirExpression, Outcome> = when (symbol)
    {
        is ThirDeclaration.Const         -> TODO()
        is ThirDeclaration.Field         -> TODO()
        is ThirDeclaration.Function      -> resolve(symbol, instance)
        is ThirDeclaration.Parameter     -> TODO()
        is ThirDeclaration.Structure     -> resolve(symbol, instance)
        is ThirDeclaration.TypeParameter -> TODO()
        is ThirDeclaration.Variable      -> TODO()
    }
    
    private fun resolve(symbol: ThirDeclaration.Function, instance: HirExpression.Identifier): Result<ThirExpression, Outcome>
    {
        val missing = (symbol.parameterIds + symbol.typeParameterIds).filter { it !in env.declarations }
        if (missing.isNotEmpty())
            return Outcome.RequireDeclaration(missing.toSet()).toFailure()
        
        val parameters = symbol.parameterIds
            .mapNotNull { env.declarations[it] }
            .filterIsInstance<ThirDeclaration.Parameter>()
        val typeParameters = symbol.typeParameterIds
            .mapNotNull { env.declarations[it] }
            .filterIsInstance<ThirDeclaration.TypeParameter>()
        
        val undefined = typeParameters.filter { it.def == null }.map { it.id }
        if (undefined.isNotEmpty())
            return Outcome.RequireDefinition(undefined.toSet()).toFailure()
        
        val foo = TypeArgumentResolver(evaluator, instance.typeParameters, typeParameters, env, scope, requireDefinition)
        val bar = foo.process().valueOr { return it.toFailure() }
        val baz = bar.map { evaluator.evaluate(it).valueOrDie()!! }
        val worker = ArgumentResolver(evaluator, node.parameters, parameters, env, scope, requireDefinition)
        
        return ThirExpression.Call(
            instance = ThirExpression.Type(ThirType.Function(symbol.id, baz, symbol.valueKind, symbol.errorKind)),
            parameters = worker.process().valueOr { return it.toFailure() },
            valueKind = symbol.valueKind,
            errorKind = symbol.errorKind,
        ).toSuccess()
    }
    
    private fun resolve(symbol: ThirDeclaration.Structure, instance: HirExpression.Identifier): Result<ThirExpression, Outcome>
    {
        val missing = symbol.fieldIds.filter { it !in env.declarations }
        if (missing.isNotEmpty())
            return Outcome.RequireDeclaration(missing.toSet()).toFailure()
        
        val ctorParams = symbol.ctorEntryIds
            .mapNotNull { env.declarations[it] }
        val typeParameters = symbol.typeParameterIds
            .mapNotNull { env.declarations[it] }
            .filterIsInstance<ThirDeclaration.TypeParameter>()
    
        val undefined = typeParameters.filter { it.def == null }.map { it.id }
        if (undefined.isNotEmpty())
            return Outcome.RequireDefinition(undefined.toSet()).toFailure()
    
        fun ThirDeclaration.fix(): ThirDeclaration.Parameter = when (this)
        {
            is ThirDeclaration.Field         -> ThirDeclaration.Parameter(id, name, Passability.MOVE, kind, null)
            is ThirDeclaration.Parameter     -> this
            else -> throw IllegalArgumentException("Temporary code is busted - symbol '$symbol', instance '$instance'")
        }
        
        val foo = TypeArgumentResolver(evaluator, instance.typeParameters, typeParameters, env, scope, requireDefinition)
        val bar = foo.process().valueOr { return it.toFailure() }
        val baz = bar.map { evaluator.evaluate(it).valueOrDie()!! }
        val worker = ArgumentResolver(evaluator, node.parameters, ctorParams.map { it.fix() }, env, scope, requireDefinition)
    
        // TODO: Handle fallible constructors. When any field raises an error, the error type must be updated to be
        //       exactly the union of all field initializer errors.
        val type = ThirType.Structure(structureId = symbol.id, typeParameters = baz)
        val call = ThirType.Function(symbol.id, baz, ThirKind.Value(type), ThirKind.Nothing)
        
        return ThirExpression.Call(
            instance = ThirExpression.Type(call),
            parameters = worker.process().valueOr { return it.toFailure() },
            valueKind = call.valueKind,
            errorKind = call.errorKind,
        ).toSuccess()
    }
}

/**
 * Converts from a HIR catch to a THIR catch.
 */
internal class CatchDefiner(
    evaluator: Evaluator,
    private val node: HirExpression.Catch,
    env: Environment,
    parentScope: Scope,
    requireDefinition: Boolean,
) : Worker<ThirExpression>
{
    private val lhsWorker = expressionDefinerOf(evaluator, node.lhs, env, parentScope, null, requireDefinition)
    private val rhsWorker = expressionDefinerOf(evaluator, node.rhs, env, parentScope, null, requireDefinition)
    
    private var lhs: ThirExpression? = null
    private var rhs: ThirExpression? = null
    
    override fun process(): Result<ThirExpression, Outcome>
    {
        if (lhs == null)
            lhs = lhsWorker.process().valueOr { return it.toFailure() }
        if (rhs == null)
            rhs = rhsWorker.process().valueOr { return it.toFailure() }
        
        val lhsValueKind = lhs!!.valueKind
        val lhsErrorKind = lhs!!.errorKind
        val rhsValueKind = rhs!!.valueKind
        val rhsErrorKind = rhs!!.errorKind
        
        if (lhsErrorKind is ThirKind.Nothing)
            return Outcome.CatchLeftHasNoError.toFailure()
        if (rhsErrorKind !is ThirKind.Nothing)
            return Outcome.CatchRightHasError(rhsErrorKind).toFailure()
        
        if (node.operator == CatchOperator.HANDLE)
        {
            if (lhsValueKind is ThirKind.Nothing && rhsValueKind !is ThirKind.Nothing)
                return Outcome.CatchHandleKindMismatch(lhsValueKind, rhsValueKind).toFailure()
            if (lhsValueKind is ThirKind.Type && rhsValueKind !is ThirKind.Type)
                return Outcome.CatchHandleKindMismatch(lhsValueKind, rhsValueKind).toFailure()
            if (lhsValueKind is ThirKind.Value && rhsValueKind !is ThirKind.Value)
                return Outcome.CatchHandleKindMismatch(lhsValueKind, rhsValueKind).toFailure()
            if (lhsValueKind is ThirKind.Value && rhsValueKind is ThirKind.Value && lhsValueKind.type != rhsValueKind.type)
                return Outcome.Unsupported("Catch unions are not yet supported").toFailure()
        }
        return ThirExpression.Catch(lhs!!, rhs!!, node.operator).toSuccess()
    }
}
