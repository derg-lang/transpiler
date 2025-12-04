package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*
import kotlin.collections.ArrayDeque

/**
 * All return statements are implemented as exceptions for simplicity. The exception carries the value which should be
 * returned from a function call.
 */
private data object ReturnException : Exception()
private data class ReturnValueException(val value: ThirExpression.Canonical?) : Exception()
private data class ReturnErrorException(val error: ThirExpression.Canonical?) : Exception()

/**
 * The interpreter is responsible for evaluating a program to its simplest form.
 */
class Interpreter(private val env: Environment)
{
    private val memory = mutableMapOf<UUID, ThirExpression.Canonical>()
    private val stack = ArrayDeque<MutableMap<UUID, ThirExpression.Canonical>>().apply { addLast(mutableMapOf()) }
    
    private fun ThirDeclaration.Function.toType(): ThirType = ThirType.Function(
        functionId = id,
        typeParameters = emptyList(),
        valueKind = valueKind,
        errorKind = errorKind,
    )
    
    private fun ThirDeclaration.Structure.toType(): ThirType = when (id)
    {
        Builtin.INT32.id   -> ThirType.Int32
        Builtin.INT64.id   -> ThirType.Int64
        Builtin.FLOAT32.id -> ThirType.Float32
        Builtin.FLOAT64.id -> ThirType.Float64
        Builtin.STR.id     -> ThirType.Str
        Builtin.BOOL.id    -> ThirType.Bool
        else               -> ThirType.Structure(id, emptyList()) // TODO: Take into consideration all generics as well.
    }
    
    /**
     * Registers a new [function] to the interpreter for testing purposes.
     */
    @Deprecated("Mark the function as something which exists in the environment instead - stop being a lazy derg!")
    fun registerFunction(function: ThirDeclaration.Function)
    {
        env.declarations[function.id] = function
    }
    
    /**
     * Computes the [expression] down to a single value. The program will either evaluate to a success or a failure,
     * depending on the input parameters. If the program raises an error, the error value will be returned as the
     * failure branch.
     */
    fun evaluate(expression: ThirExpression): Result<ThirExpression.Canonical?, ThirExpression.Canonical?> = when (expression)
    {
        is ThirExpression.Bool     -> expression.toSuccess()
        is ThirExpression.Float32  -> expression.toSuccess()
        is ThirExpression.Float64  -> expression.toSuccess()
        is ThirExpression.Int32    -> expression.toSuccess()
        is ThirExpression.Int64    -> expression.toSuccess()
        is ThirExpression.Str      -> expression.toSuccess()
        is ThirExpression.Instance -> expression.toSuccess()
        is ThirExpression.Type     -> expression.toSuccess()
        is ThirExpression.Call     -> evaluateCall(expression)
        is ThirExpression.Catch    -> evaluateCatch(expression)
        is ThirExpression.Field    -> evaluateField(expression).toSuccess()
        is ThirExpression.Load     -> evaluateLoad(expression).toSuccess()
    }
    
    private fun evaluateCall(expression: ThirExpression.Call): Result<ThirExpression.Canonical?, ThirExpression.Canonical?>
    {
        val instance = evaluate(expression.instance).valueOr { return it.toFailure() }
        val expr = instance as? ThirExpression.Type
            ?: throw IllegalStateException("Expected a type expression, but received '$instance'")
        val type = expr.raw as? ThirType.Function
            ?: throw IllegalStateException("Expected a function type, but received '${expr.raw}'")
        
        // If we are dealing with some builtin function, just deal with it in the trivial manner. No need for stack
        // frames or anything fancy in that case.
        invokeBuiltin(type.functionId, expression.parameters).onSuccess { return it.toSuccess() }
        
        // Otherwise, we figure out if what we are dealing with is a function call or a structure instantiation.
        val symbol = env.declarations[type.functionId]
        
        if (symbol is ThirDeclaration.Function)
        {
            val frame = mutableMapOf<UUID, ThirExpression.Canonical>()
            
            (symbol.typeParameterIds zip type.typeParameters).forEach { frame[it.first] = it.second }
            (symbol.parameterIds zip expression.parameters).forEach { frame[it.first] = evaluate(it.second).valueOrDie()!! }
            stack.addLast(frame)
            val outcome = try
            {
                execute(symbol.def!!.statements)
                null.toSuccess()
            }
            catch (e: ReturnException)
            {
                null.toSuccess()
            }
            catch (e: ReturnValueException)
            {
                e.value.toSuccess()
            }
            catch (e: ReturnErrorException)
            {
                e.error.toFailure()
            }
            stack.removeLast()
            return outcome
        }
        
        if (symbol is ThirDeclaration.Structure)
        {
            val frame = mutableMapOf<UUID, ThirExpression.Canonical>()
            
            (symbol.typeParameterIds zip type.typeParameters).forEach { frame[it.first] = it.second }
            (symbol.ctorEntryIds zip expression.parameters).forEach { frame[it.first] = evaluate(it.second).valueOrDie()!! }
            
            val fields = symbol.fieldIds
                .map { env.declarations[it] }
                .filterIsInstance<ThirDeclaration.Field>()
            val typeParameters = symbol.typeParameterIds
                .map { env.declarations[it] }
                .filterIsInstance<ThirDeclaration.TypeParameter>()
    
            stack.addLast(frame)
            fields.filter { it.id !in frame }.forEach { frame[it.id] = evaluate(it.def!!.default!!).valueOrDie()!! }
            stack.removeLast()
            
            return ThirExpression.Instance(
                fields = symbol.fieldIds.associateWith { frame[it]!! }.toMutableMap(),
                valueKind = ThirKind.Value(ThirType.Structure(symbol.id, typeParameters.map { frame[it.id]!! })),
            ).toSuccess()
        }
        
        throw IllegalStateException("Expected a function, but received '${env.declarations[type.functionId]}'")
    }
    
    private fun evaluateCatch(expression: ThirExpression.Catch): Result<ThirExpression.Canonical?, ThirExpression.Canonical?>
    {
        val lhs = evaluate(expression.lhs)
        
        when
        {
            lhs.isSuccess && expression.operator == CatchOperator.HANDLE       -> return lhs.valueOrNull()!!.toSuccess()
            lhs.isSuccess && expression.operator == CatchOperator.RETURN_ERROR -> return lhs.valueOrNull()!!.toSuccess()
            lhs.isFailure && expression.operator == CatchOperator.RETURN_VALUE -> return lhs.errorOrNull()!!.toSuccess()
        }
        
        // TODO: Find a way to make use of the `it` parameter.
        val rhs = evaluate(expression.rhs).valueOrDie()
        
        when (expression.operator)
        {
            CatchOperator.HANDLE       -> return rhs.toSuccess()
            CatchOperator.RETURN_ERROR -> throw ReturnErrorException(rhs)
            CatchOperator.RETURN_VALUE -> throw ReturnValueException(rhs)
        }
    }
    
    private fun evaluateField(expression: ThirExpression.Field): ThirExpression.Canonical
    {
        val structure = evaluate(expression.instance).valueOrDie().instance
        return evaluate(structure.fields[expression.fieldId]!!).valueOrDie()!!
    }
    
    private fun evaluateLoad(expression: ThirExpression.Load): ThirExpression.Canonical
    {
        val stackValue = stack.last()[expression.symbolId] ?: memory[expression.symbolId]
        if (stackValue != null)
            return stackValue
        
        val symbol = env.declarations[expression.symbolId]
            ?: throw IllegalArgumentException("Attempted to load non-existing symbol '${expression.symbolId}'")
        return when (symbol)
        {
            is ThirDeclaration.Const         -> evaluate(symbol.def!!.value).valueOrDie()!!
            is ThirDeclaration.Field         -> evaluate(symbol.def!!.default!!).valueOrDie()!!
            is ThirDeclaration.Function      -> ThirExpression.Type(symbol.toType())
            is ThirDeclaration.Parameter     -> evaluate(symbol.def!!.default!!).valueOrDie()!!
            is ThirDeclaration.Structure     -> ThirExpression.Type(symbol.toType())
            is ThirDeclaration.TypeParameter -> evaluate(symbol.def!!.default!!).valueOrDie()!!
            is ThirDeclaration.Variable      -> evaluate(symbol.def!!.value).valueOrDie()!!
        }
    }
    
    private fun execute(statements: List<ThirStatement>)
    {
        statements.forEach { execute(it) }
    }
    
    private fun execute(statement: ThirStatement)
    {
        when (statement)
        {
            is ThirStatement.Assign      -> executeAssign(statement)
            is ThirStatement.Evaluate    -> evaluate(statement.expression)
            is ThirStatement.If          -> executeIf(statement)
            is ThirStatement.Return      -> throw ReturnException
            is ThirStatement.ReturnError -> throw ReturnErrorException(evaluate(statement.expression).valueOrDie())
            is ThirStatement.ReturnValue -> throw ReturnValueException(evaluate(statement.expression).valueOrDie())
            is ThirStatement.While       -> executeWhile(statement)
        }
    }
    
    private fun executeAssign(statement: ThirStatement.Assign)
    {
        var instance = statement.instance
        while (instance !is ThirExpression.Load)
            instance = evaluate(instance).valueOrDie()!!
        
        memory[instance.symbolId] = evaluate(statement.expression).valueOrDie()!!
    }
    
    private fun executeIf(statement: ThirStatement.If)
    {
        if (evaluate(statement.predicate).valueOrDie().bool)
            execute(statement.success)
        else
            execute(statement.failure)
    }
    
    private fun executeWhile(statement: ThirStatement.While)
    {
        while (evaluate(statement.predicate).valueOrDie().bool)
            execute(statement.statements)
    }
    
    /**
     * Attempts to invoke the given [functionId] as if it was a primitive. If the function could be invoked with the
     * given [parameters], the function returns the outcome of the operation. Otherwise, if the function could not be
     * interpreted as a builtin function, this method returns an error case.
     */
    private fun invokeBuiltin(functionId: UUID, parameters: List<ThirExpression>): Result<ThirExpression.Canonical?, Unit>
    {
        val lhs = parameters.firstOrNull()?.let { evaluate(it) }?.valueOrDie()
        val rhs = parameters.lastOrNull()?.let { evaluate(it) }?.valueOrDie()
        
        return when (functionId)
        {
            Builtin.BOOL_AND.id    -> ThirExpression.Bool(lhs.bool && rhs.bool).toSuccess()
            Builtin.BOOL_EQ.id     -> ThirExpression.Bool(lhs.bool == rhs.bool).toSuccess()
            Builtin.BOOL_NE.id     -> ThirExpression.Bool(lhs.bool != rhs.bool).toSuccess()
            Builtin.BOOL_NOT.id    -> ThirExpression.Bool(!rhs.bool).toSuccess()
            Builtin.BOOL_OR.id     -> ThirExpression.Bool(lhs.bool || rhs.bool).toSuccess()
            Builtin.BOOL_XOR.id    -> ThirExpression.Bool(lhs.bool xor rhs.bool).toSuccess()
            Builtin.INT32_EQ.id    -> ThirExpression.Bool(lhs.int32 == rhs.int32).toSuccess()
            Builtin.INT32_GE.id    -> ThirExpression.Bool(lhs.int32 >= rhs.int32).toSuccess()
            Builtin.INT32_GT.id    -> ThirExpression.Bool(lhs.int32 > rhs.int32).toSuccess()
            Builtin.INT32_LE.id    -> ThirExpression.Bool(lhs.int32 <= rhs.int32).toSuccess()
            Builtin.INT32_LT.id    -> ThirExpression.Bool(lhs.int32 < rhs.int32).toSuccess()
            Builtin.INT32_NE.id    -> ThirExpression.Bool(lhs.int32 != rhs.int32).toSuccess()
            Builtin.INT32_ADD.id   -> ThirExpression.Int32(lhs.int32 + rhs.int32).toSuccess()
            Builtin.INT32_DIV.id   -> ThirExpression.Int32(lhs.int32 / rhs.int32).toSuccess()
            Builtin.INT32_MOD.id   -> ThirExpression.Int32(lhs.int32 % rhs.int32).toSuccess()
            Builtin.INT32_MUL.id   -> ThirExpression.Int32(lhs.int32 * rhs.int32).toSuccess()
            Builtin.INT32_NEG.id   -> ThirExpression.Int32(-rhs.int32).toSuccess()
            Builtin.INT32_POS.id   -> ThirExpression.Int32(+rhs.int32).toSuccess()
            Builtin.INT32_SUB.id   -> ThirExpression.Int32(lhs.int32 - rhs.int32).toSuccess()
            Builtin.INT64_EQ.id    -> ThirExpression.Bool(lhs.int64 == rhs.int64).toSuccess()
            Builtin.INT64_GE.id    -> ThirExpression.Bool(lhs.int64 >= rhs.int64).toSuccess()
            Builtin.INT64_GT.id    -> ThirExpression.Bool(lhs.int64 > rhs.int64).toSuccess()
            Builtin.INT64_LE.id    -> ThirExpression.Bool(lhs.int64 <= rhs.int64).toSuccess()
            Builtin.INT64_LT.id    -> ThirExpression.Bool(lhs.int64 < rhs.int64).toSuccess()
            Builtin.INT64_NE.id    -> ThirExpression.Bool(lhs.int64 != rhs.int64).toSuccess()
            Builtin.INT64_ADD.id   -> ThirExpression.Int64(lhs.int64 + rhs.int64).toSuccess()
            Builtin.INT64_DIV.id   -> ThirExpression.Int64(lhs.int64 / rhs.int64).toSuccess()
            Builtin.INT64_MOD.id   -> ThirExpression.Int64(lhs.int64 % rhs.int64).toSuccess()
            Builtin.INT64_MUL.id   -> ThirExpression.Int64(lhs.int64 * rhs.int64).toSuccess()
            Builtin.INT64_NEG.id   -> ThirExpression.Int64(-rhs.int64).toSuccess()
            Builtin.INT64_POS.id   -> ThirExpression.Int64(+rhs.int64).toSuccess()
            Builtin.INT64_SUB.id   -> ThirExpression.Int64(lhs.int64 - rhs.int64).toSuccess()
            Builtin.FLOAT32_EQ.id  -> ThirExpression.Bool(lhs.float32 == rhs.float32).toSuccess()
            Builtin.FLOAT32_GE.id  -> ThirExpression.Bool(lhs.float32 >= rhs.float32).toSuccess()
            Builtin.FLOAT32_GT.id  -> ThirExpression.Bool(lhs.float32 > rhs.float32).toSuccess()
            Builtin.FLOAT32_LE.id  -> ThirExpression.Bool(lhs.float32 <= rhs.float32).toSuccess()
            Builtin.FLOAT32_LT.id  -> ThirExpression.Bool(lhs.float32 < rhs.float32).toSuccess()
            Builtin.FLOAT32_NE.id  -> ThirExpression.Bool(lhs.float32 != rhs.float32).toSuccess()
            Builtin.FLOAT32_ADD.id -> ThirExpression.Float32(lhs.float32 + rhs.float32).toSuccess()
            Builtin.FLOAT32_DIV.id -> ThirExpression.Float32(lhs.float32 / rhs.float32).toSuccess()
            Builtin.FLOAT32_MOD.id -> ThirExpression.Float32(lhs.float32 % rhs.float32).toSuccess()
            Builtin.FLOAT32_MUL.id -> ThirExpression.Float32(lhs.float32 * rhs.float32).toSuccess()
            Builtin.FLOAT32_NEG.id -> ThirExpression.Float32(-rhs.float32).toSuccess()
            Builtin.FLOAT32_POS.id -> ThirExpression.Float32(+rhs.float32).toSuccess()
            Builtin.FLOAT32_SUB.id -> ThirExpression.Float32(lhs.float32 - rhs.float32).toSuccess()
            Builtin.FLOAT64_EQ.id  -> ThirExpression.Bool(lhs.float64 == rhs.float64).toSuccess()
            Builtin.FLOAT64_GE.id  -> ThirExpression.Bool(lhs.float64 >= rhs.float64).toSuccess()
            Builtin.FLOAT64_GT.id  -> ThirExpression.Bool(lhs.float64 > rhs.float64).toSuccess()
            Builtin.FLOAT64_LE.id  -> ThirExpression.Bool(lhs.float64 <= rhs.float64).toSuccess()
            Builtin.FLOAT64_LT.id  -> ThirExpression.Bool(lhs.float64 < rhs.float64).toSuccess()
            Builtin.FLOAT64_NE.id  -> ThirExpression.Bool(lhs.float64 != rhs.float64).toSuccess()
            Builtin.FLOAT64_ADD.id -> ThirExpression.Float64(lhs.float64 + rhs.float64).toSuccess()
            Builtin.FLOAT64_DIV.id -> ThirExpression.Float64(lhs.float64 / rhs.float64).toSuccess()
            Builtin.FLOAT64_MOD.id -> ThirExpression.Float64(lhs.float64 % rhs.float64).toSuccess()
            Builtin.FLOAT64_MUL.id -> ThirExpression.Float64(lhs.float64 * rhs.float64).toSuccess()
            Builtin.FLOAT64_NEG.id -> ThirExpression.Float64(-rhs.float64).toSuccess()
            Builtin.FLOAT64_POS.id -> ThirExpression.Float64(+rhs.float64).toSuccess()
            Builtin.FLOAT64_SUB.id -> ThirExpression.Float64(lhs.float64 - rhs.float64).toSuccess()
            Builtin.STR_EQ.id      -> ThirExpression.Bool(lhs.str == rhs.str).toSuccess()
            Builtin.STR_NE.id      -> ThirExpression.Bool(lhs.str != rhs.str).toSuccess()
            Builtin.STR_ADD.id     -> ThirExpression.Str(lhs.str + rhs.str).toSuccess()
            Builtin.STR_PRINTLN.id -> println(lhs.str).let { null.toSuccess() }
            else                   -> Unit.toFailure()
        }
    }
    
    private val ThirExpression.Canonical?.bool: Boolean get() = (this as ThirExpression.Bool).raw
    private val ThirExpression.Canonical?.int32: Int get() = (this as ThirExpression.Int32).raw
    private val ThirExpression.Canonical?.int64: Long get() = (this as ThirExpression.Int64).raw
    private val ThirExpression.Canonical?.float32: Float get() = (this as ThirExpression.Float32).raw
    private val ThirExpression.Canonical?.float64: Double get() = (this as ThirExpression.Float64).raw
    private val ThirExpression.Canonical?.str: String get() = (this as ThirExpression.Str).raw
    private val ThirExpression.Canonical?.instance: ThirExpression.Instance get() = this as ThirExpression.Instance
}
