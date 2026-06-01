package com.github.derg.transpiler.experimental.resolver

import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * The environment is a storage for all forms of type information. It ensures that all kind data for all elements in the
 * source code provided by the user is up-to-date with the latest constraints.
 */
class Environment
{
    private val solution = Solution(null)
    
    /**
     * Retrieves the kind stored under the given [id]. If no kinds exists under the given id, a type variable is
     * generated to represent that specific id.
     */
    operator fun get(id: UUID): ThirType =
        solution.getType(id)
    
    /**
     * Retrieves the correlation id for the given [nodeId], if any exists. A correlation id will only be available once
     * the relevant node has been constrained with a tag constraint, and has unambiguously solved.
     */
    fun getCorrelationId(nodeId: UUID): UUID? =
        solution.getCorrelationId(nodeId)
    
    /**
     * Applies the provided [constraints] to the environment. All kinds which are impacted by the constraints will be
     * adjusted to account for the new information. If the constraints results in type violations, the issues are
     * reported as the error case.
     */
    fun solve(constraints: List<Constraint>): Result<Unit, Outcome> =
        solution.solve(constraints)
    
    /**
     * Performs a verification step on all types present in the environment. The environment is considered valid if
     * there are no occurrences of type variables anywhere. The entire environment must be fully type-checked for this
     * method to return true.
     */
    fun validate(): Result<Unit, Outcome> =
        solution.validate()
}

/**
 * Stores the current state of all type information in the program. The solution can be nested inside a [parent], where
 * the solution inherits all values in the parent solution. This is necessary to handle branching and resolution of
 * overloads, where a type cannot be committed until we are sure the chosen overload is the only viable candidate.
 */
private class Solution(private val parent: Solution?)
{
    // The actual data stored per element in the source code. This collection may contain additional entries which are
    // not considered normal elements, but may instead be type variables instantiated on-the-fly to support type
    // polymorphism.
    private val types = mutableMapOf<UUID, ThirType>()
    
    // The correlation ids are used to bind a node id to some arbitrary id solved from an expression worker. These ids
    // are primarily intended to solve overload ambiguities, where it would normally be impossible to determine which
    // expression was selected based only on the resulting kind.
    private val correlationIds = mutableMapOf<UUID, UUID>()
    
    override fun toString(): String = "Solution(types=$types, correlationIds=$correlationIds)"
    
    /**
     * Retrieves the kind stored under the given [id]. If no kinds exists under the given id, a type variable is
     * generated to represent that specific id.
     */
    fun getType(id: UUID): ThirType =
        types[id]?.let { resolve(it) }?.also { types[id] = it } ?: parent?.getType(id) ?: ThirType.Var(id)
    
    /**
     * Retrieves the correlation id for the given [id], if any exists. A correlation id will only be available once the
     * relevant node has been constrained with a tag constraint, and has unambiguously solved.
     */
    fun getCorrelationId(id: UUID): UUID? =
        correlationIds[id] ?: parent?.getCorrelationId(id)
    
    /**
     * Applies the provided [constraints] to the environment. All kinds which are impacted by the constraints will be
     * adjusted to account for the new information. If the constraints results in type violations, the issues are
     * reported as the error case.
     */
    fun solve(constraints: List<Constraint>): Result<Unit, Outcome>
    {
        constraints
            .filterIsInstance<Constraint.Tag>()
            .onEach { correlationIds[it.nodeId] = it.correlationId }
        
        constraints
            .filterIsInstance<Constraint.TypeEq>()
            .mapUntilError { unify(it.lhs, it.rhs) }
            .onFailure { return it.toFailure() }
        
        // Attempt to process all overloads in order from the least branches to the most. The idea is that we want to
        // solve the options which have the least chance of cascading into other branches - we want to avoid a
        // combinatorial explosion of branches to explore.
        val oneOfs = constraints
            .filterIsInstance<Constraint.OneOf>()
            .sortedBy { it.branches.size }
            .toMutableList()
        
        while (oneOfs.isNotEmpty())
        {
            // We want to solve all constraints one branch at a time. If we can find a single branch which satisfies the
            // constraints so far, we keep it. Otherwise, the program is malformed and must be rejected.
            val head = oneOfs.removeLast()
            val successes = mutableListOf<Solution>()
            val failures = mutableListOf<Outcome>()
            for (branch in head.branches)
            {
                val solution = Solution(this)
                solution.solve(branch + oneOfs)
                    .onSuccess { successes += solution }
                    .onFailure { failures += it }
            }
            
            // Since branches can be valid in one branch but not another, we cannot commit all information we find. We
            // have to delay persisting all type information and other data we have gathered until we are absolutely
            // sure everything is a-okay. Thus, we have to nest solutions, and commit them one layer at a time. Once we
            // reach the top layer, then everything will be persisted into the main data structure.
            when (successes.size)
            {
                1    -> successes.single().commit()
                0    -> return Outcome.Placeholder("No valid overloads were found: '$failures'").toFailure()
                else -> return Outcome.Placeholder("Multiple valid overloads were found: '$successes'").toFailure()
            }
        }
        
        return Unit.toSuccess()
    }
    
    /**
     * Performs a verification step on all types present in the environment. The environment is considered valid if
     * there are no occurrences of type variables anywhere. The entire environment must be fully type-checked for this
     * method to return true.
     */
    fun validate(): Result<Unit, Outcome>
    {
        for ((id, kind) in types)
        {
            if (resolve(kind) is ThirType.Var)
                return Outcome.Placeholder("Type resolution failed for kind '$id', '$kind' is unresolved").toFailure()
        }
        return Unit.toSuccess()
    }
    
    /**
     * Commits all types in this environment to the parent environment, if any. If the environment does not have any
     * parent, this method does nothing. Note that commitment is not recursive; only the outermost solution is merged,
     * any deeper nesting must be manually committed.
     */
    fun commit()
    {
        if (parent == null)
            return
        parent.types += types
        parent.correlationIds += correlationIds
    }
    
    /**
     * Marks the [lhs] and [rhs] kinds as being the exact same kind. The environment will be updated to reflect the
     * unification operation.
     */
    private fun unify(lhs: ThirType, rhs: ThirType): Result<Unit, Outcome>
    {
        val a = resolve(lhs)
        val b = resolve(rhs)
        
        if (a == b)
            return Unit.toSuccess()
        if (a is ThirType.Var)
            return Unit.toSuccess().also { types[a.nodeId] = b }
        if (b is ThirType.Var)
            return Unit.toSuccess().also { types[b.nodeId] = a }
        
        // Promote any non-constraints into a non-erroneous channel where relevant. This means a non-fallible expression
        // may be used in places where a fallible expression is expected. We only need to perform this operation if one
        // or both sides are channel-like.
        if (a is ThirType.Channel && b is ThirType.Channel)
        {
            unify(a.valueType, b.valueType).onFailure { return it.toFailure() }
            unify(a.errorType, b.errorType).onFailure { return it.toFailure() }
            return Unit.toSuccess()
        }
        if (a is ThirType.Channel)
            return unify(a, ThirType.Channel(b, ThirType.Void))
        if (b is ThirType.Channel)
            return unify(b, ThirType.Channel(a, ThirType.Void))
        
        // At this point, we assume that all parameters are in their proper position. This allows us to quickly unify
        // all parameters, and output types as well. Here, we must perform all parameter ordering, sorting parameters by
        // their names and positions, and so on.
        if (a is ThirType.Function && b is ThirType.Function)
        {
            if (a.parameters.size != b.parameters.size)
                return Outcome.Placeholder("Arity mismatch between expected '${a.parameters}' and actual '${b.parameters}'").toFailure()
            
            // TODO: Handle name sorting and all that funny stuff here - we need additional information to resolve the
            //       order of parameters. But, at least we only have to deal with the problem in this one place.
            for ((x, y) in a.parameters zip b.parameters)
                unify(x.second, y.second).onFailure { return it.toFailure() }
            unify(a.output, b.output).onFailure { return it.toFailure() }
            return Unit.toSuccess()
        }
        
        // Here we do the hard part of unifying the type parameters. Figure out how to handle functions later, mkay?
        if (a is ThirType.Instance && b is ThirType.Instance)
            throw IllegalArgumentException("Cannot unify '$a' with '$b' just yet")
        if (a != b)
            return Outcome.Placeholder("Cannot unify '$a' with '$b' due to mismatched types").toFailure()
        
        return Unit.toSuccess()
    }
    
    /**
     * Applies all type substitutions on the [type], working recursively through it. Note that the environment will not
     * be updated as a result of this operation, since we do not know which kind should be updated in the kinds map.
     */
    private fun resolve(type: ThirType): ThirType = when (type)
    {
        is ThirType.Bool     -> ThirType.Bool
        is ThirType.Int32    -> ThirType.Int32
        is ThirType.Int64    -> ThirType.Int64
        is ThirType.Float32  -> ThirType.Float32
        is ThirType.Float64  -> ThirType.Float64
        is ThirType.Str      -> ThirType.Str
        is ThirType.Type     -> ThirType.Type
        is ThirType.Void     -> ThirType.Void
        is ThirType.Channel  ->
        {
            // Channels can be resolved to their primitive type when we have no error types. This makes the channels
            // usable in contexts where errors are not permitted, iff the channels does not permit errors.
            val valueType = resolve(type.valueType)
            val errorType = resolve(type.errorType)
            if (errorType == ThirType.Void) valueType else ThirType.Channel(valueType, errorType)
        }
        is ThirType.Function -> ThirType.Function(type.parameters.map { it.first to resolve(it.second) }, resolve(type.output))
        is ThirType.Instance -> TODO()
        is ThirType.Tuple    -> ThirType.Tuple(type.variants.map { resolve(it) })
        is ThirType.Var      -> getType(type.nodeId)
    }
}
