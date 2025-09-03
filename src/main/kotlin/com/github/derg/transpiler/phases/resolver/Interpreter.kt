package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*
import kotlin.collections.ArrayDeque

/**
 * All return statements are implemented as exceptions for simplicity. The exception carries the value which should be
 * returned from a function call.
 */
private data object ReturnException : Exception()
private data class ReturnValueException(val value: ThirExpression?) : Exception()
private data class ReturnErrorException(val error: ThirExpression?) : Exception()

/**
 * The interpreter is responsible for evaluating a program to its simplest form.
 */
class Interpreter(private val env: Environment)
{
    private val memory = mutableMapOf<UUID, ThirExpression>()
    private val stack = ArrayDeque<MutableMap<UUID, ThirExpression>>().apply { addLast(mutableMapOf()) }
    
    private fun ThirDeclaration.Structure.toType(): ThirType = when (id)
    {
        Builtin.INT32.id   -> ThirType.Int32
        Builtin.INT64.id   -> ThirType.Int64
        Builtin.FLOAT32.id -> ThirType.Float32
        Builtin.FLOAT64.id -> ThirType.Float64
        Builtin.STR.id     -> ThirType.Str
        Builtin.BOOL.id    -> ThirType.Bool
        else               -> ThirType.Structure(id)
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
    fun evaluate(expression: ThirExpression): Result<ThirExpression?, ThirExpression?>
    {
        val constants = env.declarations.values.filterIsInstance<ThirDeclaration.Const>()
        val functions = env.declarations.values.filterIsInstance<ThirDeclaration.Function>()
        val structures = env.declarations.values.filterIsInstance<ThirDeclaration.Structure>()
        
        constants.forEach { it.def?.let { def -> memory[it.id] = def.value } }
        functions.forEach { memory[it.id] = ThirExpression.Load(it.id, it.type) }
        structures.forEach { memory[it.id] = ThirExpression.Type(it.toType()) }
        
        return execute(expression)
    }
    
    private fun invoke(expression: ThirExpression.Call): Result<ThirExpression?, ThirExpression?>
    {
        val instance = execute(expression.instance).valueOr { return it.toFailure() }
        if (instance !is ThirExpression.Load)
            throw IllegalStateException("Expected load instruction, but received '$instance' instead")
        val function = env.declarations[instance.symbolId] as? ThirDeclaration.Function
            ?: throw IllegalStateException("Expected a valid function with id '${instance.symbolId}")
        
        // If we are dealing with some builtin function, just deal with it in the trivial manner. No need for stack
        // frames or anything fancy in that case.
        invokeBuiltin(function, expression.parameters).onSuccess { return it.toSuccess() }
        
        // Otherwise, we execute all statements, bailing if we encounter some form of control flow.
        val frame = mutableMapOf<UUID, ThirExpression>()
        function.parameterIds.zip(expression.parameters).forEach { frame[it.first] = execute(it.second).valueOrDie()!! }
        stack.addLast(frame)
        val outcome = try
        {
            execute(function.def!!.statements)
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
    
    private fun execute(statements: List<ThirStatement>)
    {
        statements.forEach { execute(it) }
    }
    
    private fun execute(statement: ThirStatement)
    {
        when (statement)
        {
            is ThirStatement.Evaluate    -> execute(statement.expression)
            is ThirStatement.If          -> executeIf(statement)
            is ThirStatement.Return      -> throw ReturnException
            is ThirStatement.ReturnError -> throw ReturnErrorException(execute(statement.expression).valueOrDie())
            is ThirStatement.ReturnValue -> throw ReturnValueException(execute(statement.expression).valueOrDie())
            is ThirStatement.While       -> executeWhile(statement)
        }
    }
    
    private fun executeIf(statement: ThirStatement.If)
    {
        if (execute(statement.predicate).valueOrDie().bool)
            execute(statement.success)
        else
            execute(statement.failure)
    }
    
    private fun executeWhile(statement: ThirStatement.While)
    {
        while (execute(statement.predicate).valueOrDie().bool)
            execute(statement.statements)
    }
    
    private fun execute(expression: ThirExpression): Result<ThirExpression?, ThirExpression?> = when (expression)
    {
        is ThirExpression.Bool    -> expression.toSuccess()
        is ThirExpression.Call    -> invoke(expression)
        is ThirExpression.Float32 -> expression.toSuccess()
        is ThirExpression.Float64 -> expression.toSuccess()
        is ThirExpression.Int32   -> expression.toSuccess()
        is ThirExpression.Int64   -> expression.toSuccess()
        is ThirExpression.Load    -> (stack.last()[expression.symbolId] ?: memory[expression.symbolId]).toSuccess()
        is ThirExpression.Str     -> expression.toSuccess()
        is ThirExpression.Type    -> expression.toSuccess()
    }
    
    /**
     * Attempts to invoke the given [function] as if it was a primitive. If the function could be invoked with the given
     * [parameters], the function returns the outcome of the operation. Otherwise, if the function could not be
     * interpreted as a builtin function, this method returns an error case.
     */
    private fun invokeBuiltin(function: ThirDeclaration.Function, parameters: List<ThirExpression>): Result<ThirExpression?, Unit>
    {
        val lhs = parameters.firstOrNull()?.let { execute(it) }?.valueOrDie()
        val rhs = parameters.lastOrNull()?.let { execute(it) }?.valueOrDie()
        
        return when (function.id)
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
            Builtin.INT32_POS.id   -> ThirExpression.Int32(rhs.int32).toSuccess()
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
            Builtin.INT64_POS.id   -> ThirExpression.Int64(rhs.int64).toSuccess()
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
            Builtin.FLOAT32_POS.id -> ThirExpression.Float32(rhs.float32).toSuccess()
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
            Builtin.FLOAT64_POS.id -> ThirExpression.Float64(rhs.float64).toSuccess()
            Builtin.FLOAT64_SUB.id -> ThirExpression.Float64(lhs.float64 - rhs.float64).toSuccess()
            Builtin.STR_EQ.id      -> ThirExpression.Bool(lhs.str == rhs.str).toSuccess()
            Builtin.STR_NE.id      -> ThirExpression.Bool(lhs.str != rhs.str).toSuccess()
            Builtin.STR_ADD.id     -> ThirExpression.Str(lhs.str + rhs.str).toSuccess()
            Builtin.STR_PRINTLN.id -> println(lhs.str).let { null.toSuccess() }
            else                   -> Unit.toFailure()
        }
    }
    
    private val ThirExpression?.bool: Boolean get() = (this as ThirExpression.Bool).raw
    private val ThirExpression?.int32: Int get() = (this as ThirExpression.Int32).raw
    private val ThirExpression?.int64: Long get() = (this as ThirExpression.Int64).raw
    private val ThirExpression?.float32: Float get() = (this as ThirExpression.Float32).raw
    private val ThirExpression?.float64: Double get() = (this as ThirExpression.Float64).raw
    private val ThirExpression?.str: String get() = (this as ThirExpression.Str).raw
}
