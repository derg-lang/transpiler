package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * Converts an optional HIR type and expression, into the same type and expression as a THIR representation. The type
 * and/or expression must be provided. Both cannot be null at the same time. The expression error kind must evaluate to
 * the nothing kind.
 */
internal class TypeExprResolver(
    private val evaluator: Evaluator,
    private val kind: HirKind?,
    private val expr: HirExpression?,
    private val env: Environment,
    private val scope: Scope,
    private val requireDefinition: Boolean,
)
{
    private var kindWorker: Worker<ThirKind>? = null
    private var kindOutput: ThirKind? = null
    private var exprWorker: Worker<ThirExpression>? = null
    private var exprOutput: ThirExpression? = null
    
    init
    {
        assert(kind != null || expr != null)
    }
    
    private fun resolveExpression(): Result<ThirExpression?, Outcome>
    {
        if (exprWorker == null)
            exprWorker = expr?.let { expressionDefinerOf(evaluator, it, env, scope, kindOutput, requireDefinition) }
        if (exprOutput == null)
            exprOutput = exprWorker?.process()?.valueOr { return it.toFailure() }
        return exprOutput.toSuccess()
    }
    
    /**
     * Converts the input data into a resolved type. This type can be used in later parts of the program as a source of
     * truth as to what type the node has.
     */
    fun resolveDeclaration(): Result<ThirKind, Outcome>
    {
        if (kindWorker == null)
            kindWorker = kind?.let { KindDefiner(evaluator, it, env, scope) }
        if (kindOutput == null)
            kindOutput = kindWorker?.process()?.valueOr { return it.toFailure() }
        if (kindOutput == null)
            kindOutput = resolveExpression().valueOr { return it.toFailure() }?.valueKind
        
        if (kindOutput is ThirKind.Nothing)
            return Outcome.BindingHasNoValue.toFailure()
        
        return kindOutput!!.toSuccess()
    }
    
    /**
     * Converts the input data into a resolved value, if a value was provided initially. This value represents the value
     * the node should have by default. Note that this method must only be invoked after [resolveDeclaration] has been
     * invoked.
     */
    fun resolveDefinition(): Result<ThirExpression?, Outcome>
    {
        val value = resolveExpression().valueOr { return it.toFailure() }
            ?: return null.toSuccess()
        if (value.errorKind !is ThirKind.Nothing)
            return Outcome.BindingHasError(value.errorKind).toFailure()
        
        // NOTE: We require that the declaration has been resolved before defintion, so this is never null.
        verifyMatch(kindOutput!!, value.valueKind).onFailure { return it.toFailure() }
        return value.toSuccess()
    }
    
    private fun verifyMatch(expected: ThirKind, actual: ThirKind): Result<Unit, Outcome>
    {
        if (expected is ThirKind.Type && actual !is ThirKind.Type)
            return Outcome.BindingWrongKind(expected, actual).toFailure()
        if (expected is ThirKind.Value && actual !is ThirKind.Value)
            return Outcome.BindingWrongKind(expected, actual).toFailure()
        if (expected is ThirKind.Value && actual is ThirKind.Value && expected.type != actual.type)
            return Outcome.BindingWrongType(expected.type, actual.type).toFailure()
        
        return Unit.toSuccess()
    }
}

/**
 * Converts a list of input [arguments] into a series of outputs corresponding to the provided [parameters]. The output
 * list is the input arguments converted into a THIR representation, in the same order as the parameters. The inputs may
 * be in any arbitrary order, provided they are named.
 */
// TODO: This implementation is a first-order implementation with many features lacking or fully ignored! Note that we
//       omit many details simply to get the ball rolling. We can implement the remaining features at a more fitting
//       hour.
internal class ArgumentResolver(
    private val evaluator: Evaluator,
    private val arguments: List<NamedMaybe<HirExpression>>,
    private val parameters: List<ThirDeclaration.Parameter>,
    private val env: Environment,
    private val scope: Scope,
    private val requireDefinition: Boolean,
)
{
    private var workers: TreeMap<Int, Worker<ThirExpression>>? = null
    private val outputs = mutableMapOf<Int, ThirExpression>()
    
    /**
     * Converts the input data into a properly ordered series of expressions. These expressions may then be fed into a
     * call or load expression, depending on whether the inputs were run-time or compile-time arguments.
     */
    fun process(): Result<List<ThirExpression>, Outcome>
    {
        if (workers == null)
            workers = constructWorkers().valueOr { return it.toFailure() }
        
        // Type parameters must be bound if they have not already been. This code here keeps track of which kinds are
        // bound to what type parameters, ensuring that we can parameterize over all types.
        val typeParameterBindings = mutableMapOf<UUID, ThirKind>()
        fun magic(expected: ThirKind, actual: ThirKind): ThirKind
        {
            if (expected !is ThirKind.Value)
                return expected
            if (expected.type !is ThirType.TypeParameterRef)
                return expected
            return typeParameterBindings.getOrPut(expected.type.typeParameterId) { actual }
        }
        
        for ((slot, parameter) in parameters.withIndex().filter { it.index !in outputs })
        {
            val value = process(workers!![slot], parameter).valueOr { return it.toFailure() }
            val kind = magic(parameter.kind, value.valueKind)
            
            if (value.valueKind != kind)
                return Outcome.MismatchedKind(expected = kind, received = value.valueKind).toFailure()
            if (value.errorKind != ThirKind.Nothing)
                return Outcome.MismatchedKind(expected = ThirKind.Nothing, received = value.errorKind).toFailure()
            
            outputs[slot] = value
        }
        
        return outputs.values.toList().toSuccess()
    }
    
    private fun process(worker: Worker<ThirExpression>?, parameter: ThirDeclaration.Parameter): Result<ThirExpression, Outcome>
    {
        val value = worker?.process()?.valueOr { return it.toFailure() }
        if (value != null)
            return value.toSuccess()
        
        val definition = parameter.def ?: return Outcome.RequireDefinition(setOf(parameter.id)).toFailure()
        val default = definition.default ?: return Outcome.MissingParameter(parameter.name).toFailure()
        return default.toSuccess()
    }
    
    /**
     * Constructs a collection of workers which are capable of resolving the input expressions into the types expected
     * by the parameter list.
     */
    private fun constructWorkers(): Result<TreeMap<Int, Worker<ThirExpression>>, Outcome>
    {
        // TODO: Figure out how to handle variadic parameters at some point. We can use named parameters to handle
        //       multiple variadic parameters at the same time as well.
        val workers = TreeMap<Int, Worker<ThirExpression>>()
        val nameToIndex = parameters.withIndex().associate { it.value.name to it.index }
        
        for ((index, input) in arguments.withIndex())
        {
            val (name, value) = input
            val slot = if (name == null) index else nameToIndex[name] ?: return Outcome.UnexpectedParameter(name, value).toFailure()
            if (slot in workers)
                TODO("A named and/or positional input attempt to consume the same parameter slot")
            val parameter = parameters.getOrNull(slot)
                ?: return Outcome.UnexpectedParameter(name, value).toFailure()
            
            workers[slot] = expressionDefinerOf(evaluator, value, env, scope, parameter.kind, requireDefinition)
        }
        
        return workers.toSuccess()
    }
}

/**
 * Converts a list of input type [arguments] into a series of outputs corresponding to the provided [parameters]. The
 * output list is the input arguments converted into a THIR representation, in the same order as the parameters. The
 * inputs may be in any arbitrary order, provided they are named.
 */
// TODO: This implementation is a first-order implementation with many features lacking or fully ignored! Note that we
//       omit many details simply to get the ball rolling. We can implement the remaining features at a more fitting
//       hour.
internal class TypeArgumentResolver(
    private val evaluator: Evaluator,
    private val arguments: List<NamedMaybe<HirExpression>>,
    private val parameters: List<ThirDeclaration.TypeParameter>,
    private val env: Environment,
    private val scope: Scope,
    private val requireDefinition: Boolean,
)
{
    private var workers: TreeMap<Int, Worker<ThirExpression>>? = null
    private val outputs = mutableMapOf<Int, ThirExpression>()
    
    /**
     * Converts the input data into a properly ordered series of expressions. These expressions may then be fed into a
     * call or load expression, depending on whether the inputs were run-time or compile-time arguments.
     */
    fun process(): Result<List<ThirExpression>, Outcome>
    {
        if (workers == null)
            workers = constructWorkers().valueOr { return it.toFailure() }
        
        for ((slot, parameter) in parameters.withIndex().filter { it.index !in outputs })
        {
            val value = Interpreter(env).evaluate(process(workers!![slot], parameter).valueOr { return it.toFailure() }).valueOrDie()!!
            val kind = value.valueKind
            
            if (parameter.kind is ThirKind.Type && kind !is ThirKind.Type)
                return Outcome.MismatchedKind(parameter.kind, kind).toFailure()
            if (parameter.kind is ThirKind.Value && kind !is ThirKind.Value)
                return Outcome.MismatchedKind(parameter.kind, kind).toFailure()
            if (parameter.kind is ThirKind.Value && kind is ThirKind.Value && parameter.kind.type != kind.type)
                return Outcome.MismatchedKind(parameter.kind, kind).toFailure()
            if (value.errorKind != ThirKind.Nothing)
                return Outcome.MismatchedKind(ThirKind.Nothing, value.errorKind).toFailure()
            
            outputs[slot] = value
        }
        
        return outputs.values.toList().toSuccess()
    }
    
    private fun process(worker: Worker<ThirExpression>?, parameter: ThirDeclaration.TypeParameter): Result<ThirExpression, Outcome>
    {
        val value = worker?.process()?.valueOr { return it.toFailure() }
        if (value != null)
            return value.toSuccess()
        
        val definition = parameter.def ?: return Outcome.RequireDefinition(setOf(parameter.id)).toFailure()
        val default = definition.default ?: return Outcome.MissingParameter(parameter.name).toFailure()
        return default.toSuccess()
    }
    
    /**
     * Constructs a collection of workers which are capable of resolving the input expressions into the types expected
     * by the parameter list.
     */
    private fun constructWorkers(): Result<TreeMap<Int, Worker<ThirExpression>>, Outcome>
    {
        // TODO: Figure out how to handle variadic parameters at some point. We can use named parameters to handle
        //       multiple variadic parameters at the same time as well.
        val workers = TreeMap<Int, Worker<ThirExpression>>()
        val nameToIndex = parameters.withIndex().associate { it.value.name to it.index }
        
        for ((index, input) in arguments.withIndex())
        {
            val (name, value) = input
            val slot = if (name == null) index else nameToIndex[name] ?: return Outcome.UnexpectedParameter(name, value).toFailure()
            if (slot in workers)
                TODO("A named and/or positional input attempt to consume the same parameter slot")
            val parameter = parameters.getOrNull(slot)
                ?: return Outcome.UnexpectedParameter(name, value).toFailure()
            
            workers[slot] = expressionDefinerOf(evaluator, value, env, scope, parameter.kind, requireDefinition)
        }
        
        return workers.toSuccess()
    }
}
