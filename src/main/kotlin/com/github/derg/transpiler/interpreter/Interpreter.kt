package com.github.derg.transpiler.interpreter

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*
import kotlin.collections.ArrayDeque

/**
 * An outcome of evaluation.
 */
private typealias Evaluation = Result<ThirValue?, ThirValue?>

/**
 * Super-basic interpreter which can run some code, but not all. This is an early prototype of what an interpreter could
 * look like, but it does not represent the final interpreter in the slightest.
 */
class Interpreter(private val symbols: SymbolTable)
{
    private val stack = ArrayDeque<StackFrame>()
    
    /**
     * Runs the program by starting with the [entrypoint] function.
     */
    fun run(entrypoint: UUID): Result<ThirValue?, ThirValue?>
    {
        val main = symbols.functions[entrypoint] ?: throw IllegalArgumentException("Function with id '$entrypoint' does not exist")
        
        return pushFrame { evaluate(it, main, emptyList()) }
    }
    
    /**
     * Pushes a stack frame onto the stack before evaluating the given [function]. Once the function has been evaluated,
     * the frame is popped off the stack, and the evaluation outcome is returned.
     */
    private fun <Outcome> pushFrame(function: (StackFrame) -> Outcome): Outcome
    {
        val frame = StackFrame().also { stack.addLast(it) }
        return function(frame).also { stack.removeFirst() }
    }
    
    private fun evaluate(frame: StackFrame, symbol: ThirFunction, parameters: List<ThirValue>): Evaluation
    {
        // Ensure all parameters from the outer scope is accessible to the current scope.
        parameters.withIndex().forEach { (index, value) -> frame[symbol.parameterIds[index]] = value }
        
        return evaluateInstructions(frame, symbol.instructions)
    }
    
    private fun evaluateInstructions(frame: StackFrame, instructions: List<ThirInstruction>): Evaluation
    {
        for (instruction in instructions) when (instruction)
        {
            is ThirAssign      -> frame[instruction.symbolId] = evaluateExpression(frame, instruction.expression)
            is ThirBranch      -> evaluateBranch(frame, instruction)
            is ThirEvaluate    -> evaluateExpression(frame, instruction.expression)
            is ThirReturn      -> return null.toSuccess()
            is ThirReturnError -> return evaluateExpression(frame, instruction.expression).toFailure()
            is ThirReturnValue -> return evaluateExpression(frame, instruction.expression).toSuccess()
        }
        return null.toSuccess()
    }
    
    private fun evaluateBranch(frame: StackFrame, instruction: ThirBranch): Evaluation
    {
        val predicate = evaluateExpression(frame, instruction.predicate) as ThirConstBool
        val instructions = if (predicate.raw) instruction.success else instruction.failure
        
        return evaluateInstructions(frame, instructions)
    }
    
    private fun evaluateCall(frame: StackFrame, value: ThirCall): ThirValue
    {
        val symbol = symbols.functions[(value.instance as ThirLoad).symbolId]
            ?: throw IllegalArgumentException("No function for value '$value'")
        val parameters = value.parameters.map { evaluateExpression(frame, it) }
        
        // Forgive me Father, for I have sinned.
        return when (symbol.id)
        {
            Builtin.BOOL_AND.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstBool).raw && (evaluateExpression(frame, parameters[1]) as ThirConstBool).raw)
            Builtin.BOOL_EQ.id   -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstBool).raw == (evaluateExpression(frame, parameters[1]) as ThirConstBool).raw)
            Builtin.BOOL_NE.id   -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstBool).raw != (evaluateExpression(frame, parameters[1]) as ThirConstBool).raw)
            Builtin.BOOL_NOT.id  -> ThirConstBool(!(evaluateExpression(frame, parameters[0]) as ThirConstBool).raw)
            Builtin.BOOL_OR.id   -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstBool).raw || (evaluateExpression(frame, parameters[1]) as ThirConstBool).raw)
            Builtin.BOOL_XOR.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstBool).raw xor (evaluateExpression(frame, parameters[1]) as ThirConstBool).raw)
            Builtin.INT32_ADD.id -> ThirConstInt32((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw + (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_DIV.id -> ThirConstInt32((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw / (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_EQ.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw == (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_GE.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw >= (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_GT.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw > (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_LE.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw <= (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_LT.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw < (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_MOD.id -> ThirConstInt32((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw % (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_MUL.id -> ThirConstInt32((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw * (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_NE.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw != (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT32_NEG.id -> ThirConstInt32(-(evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw)
            Builtin.INT32_POS.id -> ThirConstInt32((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw)
            Builtin.INT32_SUB.id -> ThirConstInt32((evaluateExpression(frame, parameters[0]) as ThirConstInt32).raw - (evaluateExpression(frame, parameters[1]) as ThirConstInt32).raw)
            Builtin.INT64_ADD.id -> ThirConstInt64((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw + (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_DIV.id -> ThirConstInt64((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw / (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_EQ.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw == (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_GE.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw >= (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_GT.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw > (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_LE.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw <= (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_LT.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw < (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_MOD.id -> ThirConstInt64((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw % (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_MUL.id -> ThirConstInt64((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw * (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_NE.id  -> ThirConstBool((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw != (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            Builtin.INT64_NEG.id -> ThirConstInt64(-(evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw)
            Builtin.INT64_POS.id -> ThirConstInt64((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw)
            Builtin.INT64_SUB.id -> ThirConstInt64((evaluateExpression(frame, parameters[0]) as ThirConstInt64).raw - (evaluateExpression(frame, parameters[1]) as ThirConstInt64).raw)
            else                 -> pushFrame { evaluate(it, symbol, parameters).valueOrNull() ?: TODO() }
        }
    }
    
    private fun evaluateExpression(frame: StackFrame, value: ThirValue): ThirValue = when (value)
    {
        is ThirCall       -> evaluateCall(frame, value)
        is ThirCatch      -> TODO()
        is ThirConstBool  -> value
        is ThirConstInt32 -> value
        is ThirConstInt64 -> value
        is ThirLoad       -> frame[value.symbolId]
        is ThirMember     -> TODO()
    }
}

/**
 * The stack frame holds a record of all values which have been registered in the current scope so far. These values are
 * accessible at any point in the program, as they are registered in the global scope.
 */
private class StackFrame
{
    private val values = mutableMapOf<UUID, ThirValue>()
    
    /**
     * Assigns the [value] to the variable under the given [id].
     */
    operator fun set(id: UUID, value: ThirValue)
    {
        values[id] = value
    }
    
    /**
     * Retrieves the value associated with the variable with the given [id].
     */
    operator fun get(id: UUID): ThirValue
    {
        return values[id] ?: throw IllegalArgumentException("Variable with id '$id' has not been registered")
    }
}
