package com.github.derg.transpiler.phases.interpreter

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * An expression [instance] was evaluated to something which is not a function kind.
 */
private class NotCallable(val instance: ThirExpression, val value: ThirExpression.Canonical?) :
    Exception("Cannot invoke '$instance', as it evaluate to '$value'")

/**
 * An [expression] was evaluated to nothing at where a value was expected.
 */
private class NotExpression(val expression: ThirExpression) :
    Exception("Expression '$expression' must evaluate to something, but it evaluated to nothing")

/**
 * An expression [instance] was evaluated to something which is not a structure kind.
 */
private class NotStructure(val instance: ThirExpression, val value: ThirExpression.Canonical?) :
    Exception("Cannot access fields of '$instance', as it evaluate to '$value'")

/**
 * An [expression] was evaluated to something where nothing was expected.
 */
private class IllegalExpression(val expression: ThirExpression, val value: ThirExpression.Canonical?) :
    Exception("Expression '$expression' must evaluate to nothing, but it evaluated to '$value'")

/**
 * A function [instance] was evaluated to a failure [value] instead of a success value. Instances must be a valid value
 * in order to be used.
 */
private class IllegalFunction(val instance: ThirExpression, val value: ThirExpression.Canonical?) :
    Exception("Cannot invoke '$instance', as it evaluated to a failure '$value'")

/**
 * A structure [instance] was evaluated to a failure [value] instead of a success value. Instances must be a valid value
 * in order to be used.
 */
private class IllegalStructure(val instance: ThirExpression, val value: ThirExpression.Canonical?) :
    Exception("Cannot access field of '$instance', as it evaluated to a failure '$value'")

/**
 * A [predicate] was assigned a failure [value] instead of a success value. Predicates must be given a valid value in
 * order to be used.
 */
private class IllegalPredicate(val predicate: ThirExpression, val value: ThirExpression.Canonical?) :
    Exception("Cannot use '$predicate' as a predicate, as its value is evaluated to a failure '$value'")

/**
 * A variable [instance] was assigned a failure [value] instead of a success value. Variables must be given a valid
 * value in order to be assigned.
 */
private class IllegalValue(val instance: ThirExpression, val value: ThirExpression.Canonical?) :
    Exception("Cannot assign value to variable '$instance', as its value is evaluated to a failure '$value'")

/**
 * An evaluation statement evaluated to either a success or a failure value.
 */
private class IllegalEvaluation(val value: Result<ThirExpression.Canonical, ThirExpression.Canonical>) :
    Exception("Statement evaluated to '$value' instead of nothing")

/**
 * A parameter [instance] was evaluated to a failure [value] instead of a success value. Instances must be a valid value
 * in order to be used.
 */
private class IllegalParameter(val instance: ThirExpression, val value: ThirExpression.Canonical?) :
    Exception("Cannot use '$instance' as a parameter, as it evaluated to a failure '$value'")

/**
 * An [expression] was attempted returned, but evaluated to a failure [value] instead.
 */
private class IllegalReturn(val expression: ThirExpression, val value: ThirExpression.Canonical?) :
    Exception("Cannot use '$expression' as a return value, as it evaluated to a failure '$value'")

/**
 * Attempted to access a field in an [instance] which does not exist.
 */
private class NoSuchField(val instance: ThirExpression, val fieldId: UUID) :
    Exception("Cannot access field '$fieldId', it does not exist in '$instance'")

/**
 * All return statements are implemented as exceptions for simplicity. The exception carries the value which should be
 * returned from a function call.
 */
data object EvaluatorReturn : Exception()
data class EvaluatorReturnValue(val value: ThirExpression.Canonical) : Exception()
data class EvaluatorReturnError(val error: ThirExpression.Canonical) : Exception()

/**
 * The evaluator is responsible for evaluating expressions and statements, executing any and all side effects. All state
 * which can never change is stored in the [globals] stack frame. The globals will never be modified as a consequence of
 * evaluating statements or expressions.
 */
class Evaluator(private val environment: Environment, private val globals: StackFrame)
{
    // The stack should be initialized with one dummy element to ensure that any code executed outside a function
    // context behaves as intended.
    private val stack = mutableListOf(StackFrame())
    
    /**
     * Evaluates the [expression] to its canonical form. Expressions are always reduced to their most simple
     * counterpart, if any. In the case the expression evaluates to the happy path, the evaluation is returned as the
     * success case, otherwise it is returned as the failure case.
     */
    fun evaluate(expression: ThirExpression): Result<ThirExpression.Canonical?, ThirExpression.Canonical?> = when (expression)
    {
        is ThirExpression.Canonical -> expression.toSuccess()
        is ThirExpression.Call      -> evaluate(expression)
        is ThirExpression.Catch     -> evaluate(expression).toSuccess()
        is ThirExpression.Field     -> evaluate(expression).toSuccess()
        is ThirExpression.Load      -> evaluate(expression).toSuccess()
    }
    
    private fun evaluate(expression: ThirExpression.Call): Result<ThirExpression.Canonical?, ThirExpression.Canonical?>
    {
        val instance = evaluate(expression.instance).valueOr { throw IllegalFunction(expression.instance, it) }
        if (instance !is ThirExpression.Type || instance.raw !is ThirType.Function)
            throw NotCallable(expression.instance, instance)
        
        val parameters = expression.parameters.map { param -> evaluate(param).valueOr { throw IllegalParameter(param, it) } }
        val typeParameters = instance.raw.typeParameters.map { param -> evaluate(param).valueOr { throw IllegalParameter(param, it) } }
        val lhs = parameters.firstOrNull()
        val rhs = parameters.lastOrNull()
        
        // All builtin functions must be handled explicitly, there are no implementations for these functions. We can
        // implement them directly into the compiler, and evaluate them without doing much extra faffing around.
        when (instance.raw.functionId)
        {
            Builtin.BOOL_AND.id    -> return ThirExpression.Bool(lhs.bool && rhs.bool).toSuccess()
            Builtin.BOOL_EQ.id     -> return ThirExpression.Bool(lhs.bool == rhs.bool).toSuccess()
            Builtin.BOOL_NE.id     -> return ThirExpression.Bool(lhs.bool != rhs.bool).toSuccess()
            Builtin.BOOL_NOT.id    -> return ThirExpression.Bool(!rhs.bool).toSuccess()
            Builtin.BOOL_OR.id     -> return ThirExpression.Bool(lhs.bool || rhs.bool).toSuccess()
            Builtin.BOOL_XOR.id    -> return ThirExpression.Bool(lhs.bool xor rhs.bool).toSuccess()
            Builtin.INT32_EQ.id    -> return ThirExpression.Bool(lhs.int32 == rhs.int32).toSuccess()
            Builtin.INT32_GE.id    -> return ThirExpression.Bool(lhs.int32 >= rhs.int32).toSuccess()
            Builtin.INT32_GT.id    -> return ThirExpression.Bool(lhs.int32 > rhs.int32).toSuccess()
            Builtin.INT32_LE.id    -> return ThirExpression.Bool(lhs.int32 <= rhs.int32).toSuccess()
            Builtin.INT32_LT.id    -> return ThirExpression.Bool(lhs.int32 < rhs.int32).toSuccess()
            Builtin.INT32_NE.id    -> return ThirExpression.Bool(lhs.int32 != rhs.int32).toSuccess()
            Builtin.INT32_ADD.id   -> return ThirExpression.Int32(lhs.int32 + rhs.int32).toSuccess()
            Builtin.INT32_DIV.id   -> return ThirExpression.Int32(lhs.int32 / rhs.int32).toSuccess()
            Builtin.INT32_MOD.id   -> return ThirExpression.Int32(lhs.int32 % rhs.int32).toSuccess()
            Builtin.INT32_MUL.id   -> return ThirExpression.Int32(lhs.int32 * rhs.int32).toSuccess()
            Builtin.INT32_NEG.id   -> return ThirExpression.Int32(-rhs.int32).toSuccess()
            Builtin.INT32_POS.id   -> return ThirExpression.Int32(+rhs.int32).toSuccess()
            Builtin.INT32_SUB.id   -> return ThirExpression.Int32(lhs.int32 - rhs.int32).toSuccess()
            Builtin.INT64_EQ.id    -> return ThirExpression.Bool(lhs.int64 == rhs.int64).toSuccess()
            Builtin.INT64_GE.id    -> return ThirExpression.Bool(lhs.int64 >= rhs.int64).toSuccess()
            Builtin.INT64_GT.id    -> return ThirExpression.Bool(lhs.int64 > rhs.int64).toSuccess()
            Builtin.INT64_LE.id    -> return ThirExpression.Bool(lhs.int64 <= rhs.int64).toSuccess()
            Builtin.INT64_LT.id    -> return ThirExpression.Bool(lhs.int64 < rhs.int64).toSuccess()
            Builtin.INT64_NE.id    -> return ThirExpression.Bool(lhs.int64 != rhs.int64).toSuccess()
            Builtin.INT64_ADD.id   -> return ThirExpression.Int64(lhs.int64 + rhs.int64).toSuccess()
            Builtin.INT64_DIV.id   -> return ThirExpression.Int64(lhs.int64 / rhs.int64).toSuccess()
            Builtin.INT64_MOD.id   -> return ThirExpression.Int64(lhs.int64 % rhs.int64).toSuccess()
            Builtin.INT64_MUL.id   -> return ThirExpression.Int64(lhs.int64 * rhs.int64).toSuccess()
            Builtin.INT64_NEG.id   -> return ThirExpression.Int64(-rhs.int64).toSuccess()
            Builtin.INT64_POS.id   -> return ThirExpression.Int64(+rhs.int64).toSuccess()
            Builtin.INT64_SUB.id   -> return ThirExpression.Int64(lhs.int64 - rhs.int64).toSuccess()
            Builtin.FLOAT32_EQ.id  -> return ThirExpression.Bool(lhs.float32 == rhs.float32).toSuccess()
            Builtin.FLOAT32_GE.id  -> return ThirExpression.Bool(lhs.float32 >= rhs.float32).toSuccess()
            Builtin.FLOAT32_GT.id  -> return ThirExpression.Bool(lhs.float32 > rhs.float32).toSuccess()
            Builtin.FLOAT32_LE.id  -> return ThirExpression.Bool(lhs.float32 <= rhs.float32).toSuccess()
            Builtin.FLOAT32_LT.id  -> return ThirExpression.Bool(lhs.float32 < rhs.float32).toSuccess()
            Builtin.FLOAT32_NE.id  -> return ThirExpression.Bool(lhs.float32 != rhs.float32).toSuccess()
            Builtin.FLOAT32_ADD.id -> return ThirExpression.Float32(lhs.float32 + rhs.float32).toSuccess()
            Builtin.FLOAT32_DIV.id -> return ThirExpression.Float32(lhs.float32 / rhs.float32).toSuccess()
            Builtin.FLOAT32_MOD.id -> return ThirExpression.Float32(lhs.float32 % rhs.float32).toSuccess()
            Builtin.FLOAT32_MUL.id -> return ThirExpression.Float32(lhs.float32 * rhs.float32).toSuccess()
            Builtin.FLOAT32_NEG.id -> return ThirExpression.Float32(-rhs.float32).toSuccess()
            Builtin.FLOAT32_POS.id -> return ThirExpression.Float32(+rhs.float32).toSuccess()
            Builtin.FLOAT32_SUB.id -> return ThirExpression.Float32(lhs.float32 - rhs.float32).toSuccess()
            Builtin.FLOAT64_EQ.id  -> return ThirExpression.Bool(lhs.float64 == rhs.float64).toSuccess()
            Builtin.FLOAT64_GE.id  -> return ThirExpression.Bool(lhs.float64 >= rhs.float64).toSuccess()
            Builtin.FLOAT64_GT.id  -> return ThirExpression.Bool(lhs.float64 > rhs.float64).toSuccess()
            Builtin.FLOAT64_LE.id  -> return ThirExpression.Bool(lhs.float64 <= rhs.float64).toSuccess()
            Builtin.FLOAT64_LT.id  -> return ThirExpression.Bool(lhs.float64 < rhs.float64).toSuccess()
            Builtin.FLOAT64_NE.id  -> return ThirExpression.Bool(lhs.float64 != rhs.float64).toSuccess()
            Builtin.FLOAT64_ADD.id -> return ThirExpression.Float64(lhs.float64 + rhs.float64).toSuccess()
            Builtin.FLOAT64_DIV.id -> return ThirExpression.Float64(lhs.float64 / rhs.float64).toSuccess()
            Builtin.FLOAT64_MOD.id -> return ThirExpression.Float64(lhs.float64 % rhs.float64).toSuccess()
            Builtin.FLOAT64_MUL.id -> return ThirExpression.Float64(lhs.float64 * rhs.float64).toSuccess()
            Builtin.FLOAT64_NEG.id -> return ThirExpression.Float64(-rhs.float64).toSuccess()
            Builtin.FLOAT64_POS.id -> return ThirExpression.Float64(+rhs.float64).toSuccess()
            Builtin.FLOAT64_SUB.id -> return ThirExpression.Float64(lhs.float64 - rhs.float64).toSuccess()
            Builtin.STR_EQ.id      -> return ThirExpression.Bool(lhs.str == rhs.str).toSuccess()
            Builtin.STR_NE.id      -> return ThirExpression.Bool(lhs.str != rhs.str).toSuccess()
            Builtin.STR_ADD.id     -> return ThirExpression.Str(lhs.str + rhs.str).toSuccess()
            Builtin.STR_PRINTLN.id -> return println(lhs.str).let { null.toSuccess() }
        }
        
        // If we did not find a builtin function, we will need to do something more clever. If we find a function
        // symbol, we must invoke it. If we find a structure, we must instantiate it.
        val symbol = environment.declarations[instance.raw.functionId]
        
        if (symbol is ThirDeclaration.Function)
        {
            val frame = StackFrame()
            (symbol.parameterIds zip parameters).forEach { frame[it.first] = it.second!! }
            (symbol.typeParameterIds zip typeParameters).forEach { frame[it.first] = it.second!! }
            stack += frame
            
            return try
            {
                symbol.def!!.statements.forEach { execute(it) }
                null.toSuccess()
            }
            catch (e: EvaluatorReturn)
            {
                null.toSuccess()
            }
            catch (e: EvaluatorReturnValue)
            {
                e.value.toSuccess()
            }
            catch (e: EvaluatorReturnError)
            {
                e.error.toFailure()
            }
            finally
            {
                stack.removeLast()
            }
        }
        if (symbol is ThirDeclaration.Structure)
        {
            val frame = StackFrame()
            (symbol.ctorEntryIds zip parameters).forEach { frame[it.first] = it.second!! }
            (symbol.typeParameterIds zip typeParameters).forEach { frame[it.first] = it.second!! }
            stack += frame
            
            // TODO: The ordering in which fields are initialized must be defined. Here we just select an arbitrary
            //       ordering, which may or may not be what is desired.
            val fields = symbol.fieldIds
                .mapNotNull { environment.declarations[it] as? ThirDeclaration.Field }
                .associate { it.id to if (it.id in frame) frame[it.id] else evaluate(it.def!!.default!!).valueOrDie()!! }
            val kind = ThirKind.Value(ThirType.Structure(symbol.id, emptyList()))
            
            stack.removeLast()
            return ThirExpression.Instance(fields.toMutableMap(), kind).toSuccess()
        }
        
        // If we ever end up down here, that is because we attempted to evaluate something as a callable, when it is
        // clearly not something we can invoke.
        TODO()
    }
    
    private fun evaluate(expression: ThirExpression.Catch): ThirExpression.Canonical
    {
        // TODO: Make use of the `error` value as the `it` parameter when evaluating `value`.
        val error = evaluate(expression.lhs).errorOr { return it ?: throw NotExpression(expression.lhs) } ?: throw NotExpression(expression.lhs)
        val value = evaluate(expression.rhs).valueOr { throw IllegalExpression(expression.rhs, it) } ?: throw NotExpression(expression.rhs)
        
        when (expression.operator)
        {
            CatchOperator.HANDLE       -> return value
            CatchOperator.RETURN_ERROR -> throw EvaluatorReturnError(value)
            CatchOperator.RETURN_VALUE -> throw EvaluatorReturnValue(value)
        }
    }
    
    private fun evaluate(expression: ThirExpression.Load): ThirExpression.Canonical
    {
        val currentFrame = stack.last()
        if (expression.symbolId in currentFrame)
            return currentFrame[expression.symbolId]
        return globals[expression.symbolId]
    }
    
    private fun evaluate(expression: ThirExpression.Field): ThirExpression.Canonical
    {
        val instance = evaluate(expression.instance).valueOr { throw IllegalStructure(expression.instance, it) }
        if (instance !is ThirExpression.Instance)
            throw NotStructure(expression.instance, instance)
        return instance.fields[expression.fieldId] ?: throw NoSuchField(instance, expression.fieldId)
    }
    
    /**
     * Executes the [statement], applying the side effects of the statement to the overall program state, if there are
     * any side effects.
     */
    fun execute(statement: ThirStatement) = when (statement)
    {
        is ThirStatement.Assign      -> execute(statement)
        is ThirStatement.Evaluate    -> execute(statement)
        is ThirStatement.If          -> execute(statement)
        is ThirStatement.Return      -> throw EvaluatorReturn
        is ThirStatement.ReturnError -> execute(statement)
        is ThirStatement.ReturnValue -> execute(statement)
        is ThirStatement.While       -> execute(statement)
    }
    
    private fun execute(statement: ThirStatement.Assign)
    {
        if (statement.instance !is ThirExpression.Load)
            throw IllegalStateException("Assigning to '${statement.instance}' is not supported - only loads are")
        
        stack.last()[statement.instance.symbolId] = evaluate(statement.expression)
            .valueOr { throw IllegalValue(statement.instance, it) }
            ?: throw IllegalValue(statement.instance, null)
    }
    
    private fun execute(statement: ThirStatement.Evaluate)
    {
        val expression = evaluate(statement.expression)
        
        expression.valueOrNull()?.let { throw IllegalEvaluation(it.toSuccess()) }
        expression.errorOrNull()?.let { throw IllegalEvaluation(it.toFailure()) }
    }
    
    private fun execute(statement: ThirStatement.If)
    {
        val predicate = evaluate(statement.predicate)
            .valueOr { throw IllegalPredicate(statement.predicate, it) }
        if (predicate !is ThirExpression.Bool)
            throw IllegalPredicate(statement.predicate, predicate)
        
        if (predicate.raw)
            statement.success.forEach { execute(it) }
        else
            statement.failure.forEach { execute(it) }
    }
    
    private fun execute(statement: ThirStatement.ReturnError)
    {
        val expression = evaluate(statement.expression)
            .valueOr { throw IllegalReturn(statement.expression, it) }
            ?: throw IllegalReturn(statement.expression, null)
        throw EvaluatorReturnError(expression)
    }
    
    private fun execute(statement: ThirStatement.ReturnValue)
    {
        val expression = evaluate(statement.expression)
            .valueOr { throw IllegalReturn(statement.expression, it) }
            ?: throw IllegalReturn(statement.expression, null)
        throw EvaluatorReturnValue(expression)
    }
    
    private fun execute(statement: ThirStatement.While)
    {
        while (true)
        {
            val predicate = evaluate(statement.predicate)
                .valueOr { throw IllegalPredicate(statement.predicate, it) }
            if (predicate !is ThirExpression.Bool)
                throw IllegalPredicate(statement.predicate, predicate)
            
            if (!predicate.raw)
                break
            statement.statements.forEach { execute(it) }
        }
    }
}

private val ThirExpression.Canonical?.bool: Boolean get() = (this as ThirExpression.Bool).raw
private val ThirExpression.Canonical?.int32: Int get() = (this as ThirExpression.Int32).raw
private val ThirExpression.Canonical?.int64: Long get() = (this as ThirExpression.Int64).raw
private val ThirExpression.Canonical?.float32: Float get() = (this as ThirExpression.Float32).raw
private val ThirExpression.Canonical?.float64: Double get() = (this as ThirExpression.Float64).raw
private val ThirExpression.Canonical?.str: String get() = (this as ThirExpression.Str).raw
