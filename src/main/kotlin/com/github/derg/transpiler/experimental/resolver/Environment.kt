package com.github.derg.transpiler.experimental.resolver

import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * The environment is a storage for all forms of type information. It ensures that all kind data for all elements in the
 * source code provided by the user is up-to-date with the latest constraints.
 */
class Environment
{
    // The actual data stored per element in the source code. This collection may contain additional entries which are
    // not considered normal elements, but may instead be type variables instantiated on-the-fly to support type
    // polymorphism.
    private val kinds = mutableMapOf<UUID, ThirKind>()
    
    /**
     * Retrieves the kind stored under the given [id]. If no kinds exists under the given id, a type variable is
     * generated to represent that specific id.
     */
    operator fun get(id: UUID): ThirKind =
        kinds[id]?.let { resolve(it) }?.also { kinds[id] = it } ?: ThirKind.Var(id)
    
    /**
     * Applies the provided [constraints] to the environment. All kinds which are impacted by the constraints will be
     * adjusted to account for the new information. If the constraints results in type violations, the issues are
     * reported as the error case.
     */
    fun solve(constraints: List<Constraint>): Result<Unit, Outcome>
    {
        constraints
            .filterIsInstance<Constraint.KindEq>()
            .mapUntilError { unify(it.lhs, it.rhs) }
            .onFailure { return it.toFailure() }
        
        return Unit.toSuccess()
    }
    
    /**
     * Performs a verification step on all types present in the environment. The environment is considered valid if
     * there are no occurrences of type variables anywhere. The entire environment must be fully type-checked for this
     * method to return true.
     */
    fun validate(): Result<Unit, Outcome>
    {
        for ((id, kind) in kinds)
        {
            if (resolve(kind) is ThirKind.Var)
                return Outcome.Placeholder("Type resolution failed for kind '$id', '$kind' is unresolved").toFailure()
        }
        return Unit.toSuccess()
    }
    
    /**
     * Marks the [lhs] and [rhs] kinds as being the exact same kind. The environment will be updated to reflect the
     * unification operation.
     */
    private fun unify(lhs: ThirKind, rhs: ThirKind): Result<Unit, Outcome>
    {
        val a = resolve(lhs)
        val b = resolve(rhs)
        
        if (a == b)
            return Unit.toSuccess()
        if (a is ThirKind.Var)
            return Unit.toSuccess().also { kinds[a.nodeId] = b }
        if (b is ThirKind.Var)
            return Unit.toSuccess().also { kinds[b.nodeId] = a }
        
        if (a !is ThirKind.Value || b !is ThirKind.Value)
            return Outcome.Placeholder("Cannot unify '$a' with '$b' due to mismatched kinds").toFailure()
        
        // Here we do the hard part of unifying the type parameters. Figure out how to handle functions later, mkay?
        val x = a.type as? ThirType.Instance ?: throw IllegalArgumentException("Cannot unify '$a' just yet")
        val y = b.type as? ThirType.Instance ?: throw IllegalArgumentException("Cannot unify '$b' just yet")
        
        TODO()
    }
    
    /**
     * Applies all type substitutions on the [kind], working recursively through it. Note that the environment will not
     * be updated as a result of this operation, since we do not know which kind should be updated in the kinds map.
     */
    private fun resolve(kind: ThirKind): ThirKind = when (kind)
    {
        is ThirKind.Nothing -> kind
        is ThirKind.Type    -> kind
        is ThirKind.Value   ->
        {
            val type = when (kind.type)
            {
                is ThirType.Bool     -> ThirType.Bool
                is ThirType.Int32    -> ThirType.Int32
                is ThirType.Int64    -> ThirType.Int64
                is ThirType.Float32  -> ThirType.Float32
                is ThirType.Float64  -> ThirType.Float64
                is ThirType.Str      -> ThirType.Str
                is ThirType.Instance -> TODO()
                is ThirType.Function -> TODO()/*ThirType.Function(
                    parameters = type.parameters.map { resolve(it) },
                    valueKind = resolve(type.valueKind),
                    errorKind = resolve(type.errorKind),
                )*/
            }
            ThirKind.Value(type)
        }
        is ThirKind.Var     ->
        {
            val current = kinds[kind.nodeId] ?: kind
            if (current is ThirKind.Var && current.nodeId == kind.nodeId) current else resolve(current)
        }
    }
}
