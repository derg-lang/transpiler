package com.github.derg.transpiler.experimental.toy_solver

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.utils.*
import java.util.*

sealed interface Type
data object TNothing : Type
data object TInt : Type
data object TFloat : Type
data object TBool : Type
data object TString : Type
data object TType : Type
data class TUnion(val variants: List<Type>) : Type
data class TRecord(val fields: Map<String, Type>) : Type
data class TFun(val parameters: List<Type>, val returnType: Type) : Type
data class TVar(val typeId: UUID) : Type
data class TConstructor(val structureId: UUID, val arguments: List<Type>) : Type

private sealed interface Constraint
{
    /**
     * Indicates that two types are exactly identical.
     */
    data class TypeEq(val lhs: Type, val rhs: Type) : Constraint
    
    /**
     * Indicates that the [sub] type is contained fully in the [sup] type. This is useful for dealing with unions, where
     * we want to assign an `Int` to an `Int | Bool`.
     */
    data class TypeLeq(val sub: Type, val sup: Type) : Constraint
    
    /**
     * Exactly one of the given [alternatives] must succeed for type inference to pass. The collection of alternatives
     * are used to represent the possible overloads which are available when invoking a function, or looking up an
     * identifier by name.
     */
    data class OneOf(val alternatives: List<List<Constraint>>) : Constraint
}

/**
 * The environment stores all symbols which are currently known in the program.
 *
 * TODO: Note that this is a placeholder solution, we need to handle scopes to sort out name resolution.
 */
class Environment
{
    val functions = mutableMapOf<String, MutableList<Hir.FunctionDecl>>()
    val structures = mutableMapOf<String, Hir.StructureDecl>()
    val typeParameters = mutableMapOf<String, Hir.TypeParameterDecl>()
}

/**
 * Stores the current state of all type information in the program. The solution can be nested inside a [parent], where
 * the solution inherits all values in the parent solution. This is necessary to handle branching and resolution of
 * overloads, where a type cannot be committed until we are sure the chosen overload is the only viable candidate.
 */
class Solution(private val parent: Solution?)
{
    private val _types = mutableMapOf<UUID, Type>()
    
    val types: Map<UUID, Type>
        get() = _types
    
    /**
     * Generates a new type variable and inserts it into the type table. This variable should be used for all types
     * which are currently unknown; they will be resolved at a later time.
     */
    fun generateTypeVar(): Type = TVar(UUID.randomUUID()).also { _types[it.typeId] = it }
    
    /**
     * Determines what the type of the element with the given [id] is. If the current solution does not contain the
     * element, we attempt to look up the type in the parent, recursively.
     */
    operator fun get(id: UUID): Type? = _types[id] ?: parent?.get(id)
    
    /**
     * Marks the element with the given [id] as resolved with the given [type]. Note that the effects of this
     * substitution may not take effect until the solution is merged to the parent, if a parent is provided. See the
     * [commit] method for further details.
     */
    operator fun set(id: UUID, type: Type) = _types.set(id, type)
    
    /**
     * Commits all types in this solution to the parent solution, if any. If the solution does not have any parent, this
     * method does nothing. Note that commitment is not recursive; only the outermost solution is merged, any deeper
     * nesting must be manually committed.
     */
    fun commit()
    {
        if (parent != null)
            parent._types += _types
    }
}

/**
 * The solver is responsible for holding the state of the entire program as it is being type-checked.
 */
class Solver(environment: Environment)
{
    private val root = Solution(null)
    private val generator = ConstraintGenerator(environment, root)
    
    fun infer(expr: Hir.Expr): Result<Type, String>
    {
        val (kind, constraints) = generator.constrain(expr).valueOr { return it.toFailure() }
        solve(constraints, root).valueOr { return it.toFailure() }
        return resolve(kind, root).toSuccess()
    }
}

private class ConstraintGenerator(private val environment: Environment, private val solution: Solution)
{
    fun constrain(expr: Hir.Expr): Result<Pair<Type, List<Constraint>>, String> = when (expr)
    {
        is Hir.BoolLiteral   -> Success(TBool to emptyList())
        is Hir.FloatLiteral  -> Success(TFloat to emptyList())
        is Hir.IntLiteral    -> Success(TInt to emptyList())
        is Hir.StringLiteral -> Success(TString to emptyList())
        is Hir.Call          -> constrainCall(expr)
        is Hir.FieldAccess   -> TODO()
        is Hir.If            -> TODO()
        is Hir.Is            -> TODO()
        is Hir.NameReference -> TODO()
    }
    
    private fun constrainCall(expr: Hir.Call): Result<Pair<Type, List<Constraint>>, String>
    {
        val callType = solution.generateTypeVar()
        val (argTypes, argConstraints) = expr.arguments
            .mapUntilError { constrain(it.value) }
            .valueOr { return it.toFailure() }
            .unzip()
        
        // TODO: Somehow figure out how to accept functions as first-class citizens
        if (expr.callee !is Hir.NameReference)
            return "We are currently not supporting function on the form '${expr.callee}' - must be a name reference".toFailure()
        
        val alternatives = mutableListOf<List<Constraint>>()
        for (overload in environment.functions[expr.callee.name].orEmpty())
        {
            val constraints = mutableListOf<Constraint>()
            val instantiation = mutableMapOf<UUID, Type>()
            
            // Type parameter constraints.
            if (overload.typeParameters.size < expr.callee.typeArguments.size)
                return "Mismatching number of type parameters in '${overload.typeParameters}' and '${expr.callee.typeArguments}'".toFailure()
            
            for ((expected, actual) in overload.typeParameters zip expr.callee.typeArguments)
            {
                val parameterType = when (expected.kind)
                {
                    is Hir.KAuto    -> TODO()
                    is Hir.KNothing -> TODO()
                    is Hir.KType    -> solution.generateTypeVar()
                    is Hir.KValue   -> TODO()
                    null            -> TODO()
                }
//                val parameterType = environment.typeOf(actual.value, instantiation, solution).valueOr { return it.toFailure() }
//                    ?: return "Could not determine type of '${actual.value}'".toFailure()
                
                instantiation[expected.id] = parameterType
                constraints += Constraint.TypeEq(instantiate(TVar(expected.id), instantiation, solution), parameterType)
            }
            
            for (param in overload.typeParameters.drop(expr.callee.typeArguments.size))
            {
                val typeId = solution.generateTypeVar()
                instantiation[param.id] = typeId
                constraints += Constraint.TypeEq(instantiate(TVar(param.id), instantiation, solution), typeId)
            }
            
            // Parameter constraints.
            if (overload.parameters.size != argTypes.size)
                return "Mismatching number of parameters in '${overload.parameters}' and '$argTypes'".toFailure()
            
            for ((expected, actual) in overload.parameters zip argTypes)
            {
                if (expected.kind != null)
                {
                    val parameterType = environment.typeOf(expected.kind, instantiation, solution)
                        .valueOr { return it.toFailure() } ?: solution.generateTypeVar()
                    constraints += Constraint.TypeEq(actual, instantiate(parameterType, instantiation, solution))
                }
                if (expected.default != null)
                {
                    val (parameterType, parameterConstraints) = constrain(expected.default).valueOr { return it.toFailure() }
                    constraints += parameterConstraints
                    constraints += Constraint.TypeEq(actual, instantiate(parameterType, instantiation, solution))
                }
            }
            
            // Return type constraint.
            val returnType = environment.typeOf(overload.returnKind, instantiation, solution)
                .valueOr { return it.toFailure() } ?: solution.generateTypeVar()
            constraints += Constraint.TypeEq(callType, instantiate(returnType, instantiation, solution))
            
            alternatives += constraints
        }
        
        return Success(callType to argConstraints.flatten() + Constraint.OneOf(alternatives))
    }
}

/**
 * Converts the [kind] to a type, if the referenced type exists in [this] environment.
 */
private fun Environment.typeOf(kind: Hir.Kind, instantiation: MutableMap<UUID, Type>, solution: Solution): Result<Type?, String> = when (kind)
{
    is Hir.KAuto    -> null.toSuccess()
    is Hir.KNothing -> TNothing.toSuccess()
    is Hir.KType    -> TType.toSuccess()
    is Hir.KValue   -> typeOf(kind.type, instantiation, solution)
}

private fun Environment.typeOf(type: Hir.Expr, instantiation: MutableMap<UUID, Type>, solution: Solution): Result<Type?, String>
{
    if (type !is Hir.NameReference)
        return "We are currently not supporting structures on the form '$type' - must be a name reference".toFailure()
    if (type.typeArguments.isNotEmpty())
        return "Type parameters on '$type' is currently not supported".toFailure()
    
    val typeParameter = typeParameters[type.name]
    if (typeParameter != null)
        return TVar(typeParameter.id).toSuccess()
    
    val structure = structures[type.name] ?: return "No structures with the name '${type.name}' was found".toFailure()
    return when (structure.id)
    {
        Builtin.BOOL.id    -> TBool.toSuccess()
        Builtin.INT32.id   -> TInt.toSuccess()
        Builtin.FLOAT32.id -> TFloat.toSuccess()
        Builtin.STR.id     -> TString.toSuccess()
        else               -> TConstructor(structure.id, emptyList()).toSuccess()
    }
}

private fun solve(constraints: List<Constraint>, current: Solution): Result<Solution, String>
{
    for (constraint in constraints.filterIsInstance<Constraint.TypeEq>())
    {
        unify(constraint.lhs, constraint.rhs, current).valueOr { return it.toFailure() }
    }
    
    for (constraint in constraints.filterIsInstance<Constraint.TypeLeq>())
    {
        TODO()
    }
    
    val oneOfs = constraints.filterIsInstance<Constraint.OneOf>().toMutableList()
    while (oneOfs.isNotEmpty())
    {
        val head = oneOfs.removeLast()
        val outcomes = mutableListOf<Result<Solution, String>>()
        for (alternative in head.alternatives)
            outcomes += solve(alternative + oneOfs, Solution(current))
        
        val (successes, failures) = outcomes.partitionOutcomes()
        when (successes.size)
        {
            1    -> successes.single().commit()
            0    -> return "No valid overloads were found: '$failures'".toFailure()
            else -> return "Multiple valid overloads were found: '$failures'".toFailure()
        }
    }
    
    for ((id, type) in current.types)
    {
        if (type is TVar)
            return "Unresolved type variable $id (inferred type too generic or insufficient constraints)".toFailure()
    }
    return current.toSuccess()
}

private fun resolve(type: Type, current: Solution): Type = when (type)
{
    is TNothing     -> type
    is TBool        -> type
    is TFloat       -> type
    is TInt         -> type
    is TString      -> type
    is TType        -> type
    is TConstructor -> TConstructor(type.structureId, type.arguments.map { resolve(it, current) })
    is TFun         -> TODO()
    is TRecord      -> TODO()
    is TUnion       -> TODO()
    is TVar         -> current[type.typeId]?.let { if (it !is TVar) resolve(it, current) else it } ?: type
}

private fun instantiate(type: Type, instance: MutableMap<UUID, Type>, solution: Solution): Type = when (type)
{
    is TNothing     -> type
    is TBool        -> type
    is TFloat       -> type
    is TInt         -> type
    is TString      -> type
    is TType        -> type
    is TConstructor -> TConstructor(type.structureId, type.arguments.map { instantiate(it, instance, solution) })
    is TFun         -> TODO()
    is TRecord      -> TODO()
    is TUnion       -> TODO()
    is TVar         -> instance.getOrPut(type.typeId) { solution.generateTypeVar() }
}

private fun unify(lhs: Type, rhs: Type, current: Solution): Result<Unit, String>
{
    val a = resolve(lhs, current)
    val b = resolve(rhs, current)
    
    if (a == b)
        return Unit.toSuccess()
    if (a is TVar)
        return Unit.toSuccess().also { current[a.typeId] = b }
    if (b is TVar)
        return Unit.toSuccess().also { current[b.typeId] = a }
    
    if (a is TConstructor && b is TConstructor && a.structureId == b.structureId && a.arguments.size == b.arguments.size)
    {
        for ((x, y) in a.arguments zip b.arguments)
            unify(x, y, current).onFailure { return it.toFailure() }
    }
    
    return "Type error: cannot unify '$a' with '$b'".toFailure()
}
