package com.github.derg.transpiler.experimental.resolver.workers

import com.github.derg.transpiler.experimental.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.utils.*
import java.math.*
import java.util.*

private val INT32_MIN = Int.MIN_VALUE.toBigInteger()
private val INT32_MAX = Int.MAX_VALUE.toBigInteger()
private val INT64_MIN = Long.MIN_VALUE.toBigInteger()
private val INT64_MAX = Long.MAX_VALUE.toBigInteger()
private val FLOAT32_MIN = Float.MIN_VALUE.toBigDecimal()
private val FLOAT32_MAX = Float.MAX_VALUE.toBigDecimal()
private val FLOAT64_MIN = Double.MIN_VALUE.toBigDecimal()
private val FLOAT64_MAX = Double.MAX_VALUE.toBigDecimal()

private fun BigInteger.asInt32(): Result<ThirExpression, Outcome>
{
    if (this < INT32_MIN || this > INT32_MAX)
        return Outcome.Placeholder("The integer '$this' does not fit in an int32").toFailure()
    return ThirExpression.Int32(toInt()).toSuccess()
}

private fun BigInteger.asInt64(): Result<ThirExpression, Outcome>
{
    if (this < INT64_MIN || this > INT64_MAX)
        return Outcome.Placeholder("The integer '$this' does not fit in an int64").toFailure()
    return ThirExpression.Int64(toLong()).toSuccess()
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

private fun String.asStr(): Result<ThirExpression, Outcome>
{
    // TODO: Verify that the input string is within legal limits.
    return ThirExpression.Str(this).toSuccess()
}

/**
 * Simple shorthand for representing a primitive type and expression bound together.
 */
private typealias Primitive = Pair<ThirType, ThirExpression>

/**
 * The expression worker is responsible for generating the constraints which are applicable to the provided [node]. The
 * worker will also ensure that the [scope] is respected as well, such that it is not possible to access information
 * provided in a far-away part of the codebase.
 */
class WorkerExpression(
    private val scope: Scope,
    private val node: HirExpression,
) : Worker
{
    internal val expressions = mutableMapOf<UUID, ThirExpression>()
    
    override val id: UUID
        get() = node.id
    
    override fun generateWorkers(): List<Worker> = when (node)
    {
        is HirExpression.Bool       -> emptyList()
        is HirExpression.Decimal    -> emptyList()
        is HirExpression.Integer    -> emptyList()
        is HirExpression.Text       -> emptyList()
        is HirExpression.Call       ->
        {
            val instance = WorkerExpression(scope, node.instance)
            val parameters = node.parameters.map { WorkerExpression(scope, it.second) }
            
            parameters + instance
        }
        is HirExpression.Catch      -> TODO()
        is HirExpression.Field      -> TODO()
        is HirExpression.Identifier -> node.typeParameters.map { WorkerExpression(scope, it.second) }
        is HirExpression.Callable   -> TODO()
    }
    
    override fun generateConstraints(context: Context): Result<List<Constraint>, Outcome> = when (node)
    {
        is HirExpression.Bool       -> generateForPrimitive(context, node, ::handlerBool)
        is HirExpression.Decimal    -> generateForPrimitive(context, node, ::handlerDecimal)
        is HirExpression.Integer    -> generateForPrimitive(context, node, ::handlerInteger)
        is HirExpression.Text       -> generateForPrimitive(context, node, ::handlerString)
        is HirExpression.Call       -> constrainCall(context, node)
        is HirExpression.Catch      -> TODO()
        is HirExpression.Field      -> TODO()
        is HirExpression.Identifier -> constrainIdentifier(context, node)
        is HirExpression.Callable   -> TODO()
    }
    
    /**
     * Generates the relevant expressions and constraints applying to a primitive [node]. These nodes should only be the
     * basic ones such as booleans, integers, decimals, strings, and any other of similar nature.
     */
    private fun <Type : HirExpression> generateForPrimitive(
        context: Context,
        node: Type,
        handler: (Type) -> Result<Primitive, Outcome>,
    ): Result<List<Constraint>, Outcome>
    {
        val correlationId = UUID.randomUUID()
        val kind = context.environment[node.id]
        val (type, expression) = handler(node).valueOr { return it.toFailure() }
        
        expressions[correlationId] = expression
        
        return listOf(
            Constraint.Tag(node.id, correlationId),
            Constraint.TypeEq(kind, type),
        ).toSuccess()
    }
    
    private fun handlerBool(node: HirExpression.Bool): Result<Primitive, Outcome> =
        (ThirType.Bool to ThirExpression.Bool(node.value)).toSuccess()
    
    private fun handlerInteger(node: HirExpression.Integer): Result<Primitive, Outcome> = when (node.literal)
    {
        INT32_LIT_NAME -> node.value.asInt32().mapValue { ThirType.Int32 to it }
        INT64_LIT_NAME -> node.value.asInt64().mapValue { ThirType.Int64 to it }
        // TODO: Remove this else clause by fully defining in the language all possible literals.
        else           -> throw IllegalArgumentException("Unexpected integer literal '${node.literal}'")
    }
    
    private fun handlerDecimal(node: HirExpression.Decimal): Result<Primitive, Outcome> = when (node.literal)
    {
        FLOAT32_LIT_NAME -> node.value.asFloat32().mapValue { ThirType.Float32 to it }
        FLOAT64_LIT_NAME -> node.value.asFloat64().mapValue { ThirType.Float64 to it }
        // TODO: Remove this else clause by fully defining in the language all possible literals.
        else             -> throw IllegalArgumentException("Unexpected decimal literal '${node.literal}'")
    }
    
    private fun handlerString(node: HirExpression.Text): Result<Primitive, Outcome> = when (node.literal)
    {
        STR_LIT_NAME -> node.value.asStr().mapValue { ThirType.Str to it }
        // TODO: Remove this else clause by fully defining in the language all possible literals.
        else         -> throw IllegalArgumentException("Unexpected decimal literal '${node.literal}'")
    }
    
    /**
     * Generates the relevant expressions and constraints applying to an identifier [node]. The identifiers are
     * potentially associated with many expressions, without us knowing ahead of time which one will be selected. Thus,
     * identifiers heavily rely on correlation ids to uniquely determine what the resolution outcome was in the end.
     */
    private fun constrainIdentifier(context: Context, node: HirExpression.Identifier): Result<List<Constraint>, Outcome>
    {
        val kind = context.environment[node.id]
        val branches = mutableListOf<List<Constraint>>()
        
        for (candidateId in scope.find(node.name))
        {
            val candidate = context.declarations[candidateId]
                ?: throw IllegalStateException("Could not find candidate '$candidateId' in environment")
            
            when (candidate)
            {
                is ThirDeclaration.Constant      -> TODO()
                is ThirDeclaration.Field         -> TODO()
                is ThirDeclaration.Function      ->
                {
                    val nodeType = context.environment[node.id]
                    val funcType = context.environment[candidate.id]
                    
                    expressions[candidateId] = ThirExpression.Function(candidate.id)
                    branches += listOf(Constraint.Tag(node.id, candidateId), Constraint.TypeEq(nodeType, funcType))
                }
                is ThirDeclaration.Method        -> TODO()
                is ThirDeclaration.Parameter     -> TODO()
                is ThirDeclaration.Structure     ->
                {
                    val type = when (candidateId)
                    {
                        Builtin.BOOL.id    -> ThirType.Bool
                        Builtin.INT32.id   -> ThirType.Int32
                        Builtin.INT64.id   -> ThirType.Int64
                        Builtin.FLOAT32.id -> ThirType.Float32
                        Builtin.FLOAT64.id -> ThirType.Float32
                        Builtin.STR.id     -> ThirType.Str
                        else               -> ThirType.Instance(candidateId, emptyList())
                    }
                    
                    expressions[candidateId] = ThirExpression.Type(type)
                    branches += listOf(Constraint.Tag(node.id, candidateId), Constraint.TypeEq(kind, ThirType.Type))
                }
                is ThirDeclaration.TypeParameter -> TODO()
                is ThirDeclaration.Variable      -> TODO()
            }
        }
        
        if (branches.isEmpty())
            return Outcome.Placeholder("No viable candidate was found for node '$node' in scope").toFailure()
        if (branches.size == 1)
            return branches.single().toSuccess()
        return listOf(Constraint.OneOf(branches)).toSuccess()
    }
    
    /**
     * Generates the relevant constraints applying to a function application. The constraint limits which parameters and
     * return values and errors are permitted on call site.
     */
    private fun constrainCall(context: Context, node: HirExpression.Call): Result<List<Constraint>, Outcome>
    {
        val type = ThirType.Function(
            node.parameters.map { it.first to context.environment[it.second.id] },
            context.environment[node.id],
        )
        
        val constraint = Constraint.TypeEq(context.environment[node.instance.id], type)
        return listOf(constraint).toSuccess()
    }
}

/*
fun foo(): Int32 { return 0 }
fun foo(): type { return Int32 }

fun bar(in test: type) {}

bar(foo()) // Deduces that `foo()` is `type`, meaning `foo` must be deduced to `() -> type`, solving the overload
*/
