package com.github.derg.transpiler.interpreter

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*
import kotlin.collections.ArrayDeque

/**
 * An outcome of evaluation.
 *
 * @param outcome The value the expression was evaluated to.
 * @param return Whether the current function call should immediately return or not.
 */
private data class Evaluation(val outcome: Result<ThirValue?, ThirValue?>, val `return`: Boolean)

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
        
        return pushFrame { evaluate(it, main, emptyList()) }.outcome
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
            is ThirBranch      -> evaluateBranch(frame, instruction).also { if (it.`return`) return it }
            is ThirEvaluate    -> evaluateExpression(frame, instruction.expression)
            is ThirReturn      -> return Evaluation(null.toSuccess(), true)
            is ThirReturnError -> return Evaluation(evaluateExpression(frame, instruction.expression).toFailure(), true)
            is ThirReturnValue -> return Evaluation(evaluateExpression(frame, instruction.expression).toSuccess(), true)
            is ThirWhile       -> evaluateWhile(frame, instruction.predicate, instruction.instructions)?.also { if (it.`return`) return it }
        }
        return Evaluation(null.toSuccess(), false)
    }
    
    private fun evaluateBranch(frame: StackFrame, instruction: ThirBranch): Evaluation
    {
        val predicate = evaluateExpression(frame, instruction.predicate) as ThirConstBool
        val instructions = if (predicate.raw) instruction.success else instruction.failure
        
        return evaluateInstructions(frame, instructions)
    }
    
    private fun evaluateExpression(frame: StackFrame, value: ThirValue): ThirValue = when (value)
    {
        is ThirCall       -> evaluateCall(frame, value)
        is ThirCatch      -> TODO()
        is ThirConstBool  -> value
        is ThirConstInt32 -> value
        is ThirConstInt64 -> value
        is ThirConstStr   -> value
        is ThirLoad       -> frame[value.symbolId]
        is ThirMember     -> (evaluateExpression(frame, value.instance) as ThirRecord).fields[value.fieldId]!!
        is ThirRecord     -> value.apply { fields.forEach { (id, value) -> fields[id] = evaluateExpression(frame, value) } }
    }
    
    private fun evaluateCall(frame: StackFrame, value: ThirCall): ThirValue
    {
        val symbol = symbols.functions[(value.instance as ThirLoad).symbolId]
            ?: throw IllegalArgumentException("No function for value '$value'")
        
        // Forgive me Father, for I have sinned...
        val parameters = value.parameters.map { evaluateExpression(frame, it) }
        val a = parameters.getOrNull(0)
        val b = parameters.getOrNull(1)
        
        return when (symbol.id)
        {
            Builtin.BOOL_AND.id  -> ThirConstBool((a as ThirConstBool).raw && (b as ThirConstBool).raw)
            Builtin.BOOL_EQ.id   -> ThirConstBool((a as ThirConstBool).raw == (b as ThirConstBool).raw)
            Builtin.BOOL_NE.id   -> ThirConstBool((a as ThirConstBool).raw != (b as ThirConstBool).raw)
            Builtin.BOOL_NOT.id  -> ThirConstBool(!(a as ThirConstBool).raw)
            Builtin.BOOL_OR.id   -> ThirConstBool((a as ThirConstBool).raw || (b as ThirConstBool).raw)
            Builtin.BOOL_XOR.id  -> ThirConstBool((a as ThirConstBool).raw xor (b as ThirConstBool).raw)
            Builtin.INT32_ADD.id -> ThirConstInt32((a as ThirConstInt32).raw + (b as ThirConstInt32).raw)
            Builtin.INT32_DIV.id -> ThirConstInt32((a as ThirConstInt32).raw / (b as ThirConstInt32).raw)
            Builtin.INT32_EQ.id  -> ThirConstBool((a as ThirConstInt32).raw == (b as ThirConstInt32).raw)
            Builtin.INT32_GE.id  -> ThirConstBool((a as ThirConstInt32).raw >= (b as ThirConstInt32).raw)
            Builtin.INT32_GT.id  -> ThirConstBool((a as ThirConstInt32).raw > (b as ThirConstInt32).raw)
            Builtin.INT32_LE.id  -> ThirConstBool((a as ThirConstInt32).raw <= (b as ThirConstInt32).raw)
            Builtin.INT32_LT.id  -> ThirConstBool((a as ThirConstInt32).raw < (b as ThirConstInt32).raw)
            Builtin.INT32_MOD.id -> ThirConstInt32((a as ThirConstInt32).raw % (b as ThirConstInt32).raw)
            Builtin.INT32_MUL.id -> ThirConstInt32((a as ThirConstInt32).raw * (b as ThirConstInt32).raw)
            Builtin.INT32_NE.id  -> ThirConstBool((a as ThirConstInt32).raw != (b as ThirConstInt32).raw)
            Builtin.INT32_NEG.id -> ThirConstInt32(-(a as ThirConstInt32).raw)
            Builtin.INT32_POS.id -> ThirConstInt32((a as ThirConstInt32).raw)
            Builtin.INT32_SUB.id -> ThirConstInt32((a as ThirConstInt32).raw - (b as ThirConstInt32).raw)
            Builtin.INT64_ADD.id -> ThirConstInt64((a as ThirConstInt64).raw + (b as ThirConstInt64).raw)
            Builtin.INT64_DIV.id -> ThirConstInt64((a as ThirConstInt64).raw / (b as ThirConstInt64).raw)
            Builtin.INT64_EQ.id  -> ThirConstBool((a as ThirConstInt64).raw == (b as ThirConstInt64).raw)
            Builtin.INT64_GE.id  -> ThirConstBool((a as ThirConstInt64).raw >= (b as ThirConstInt64).raw)
            Builtin.INT64_GT.id  -> ThirConstBool((a as ThirConstInt64).raw > (b as ThirConstInt64).raw)
            Builtin.INT64_LE.id  -> ThirConstBool((a as ThirConstInt64).raw <= (b as ThirConstInt64).raw)
            Builtin.INT64_LT.id  -> ThirConstBool((a as ThirConstInt64).raw < (b as ThirConstInt64).raw)
            Builtin.INT64_MOD.id -> ThirConstInt64((a as ThirConstInt64).raw % (b as ThirConstInt64).raw)
            Builtin.INT64_MUL.id -> ThirConstInt64((a as ThirConstInt64).raw * (b as ThirConstInt64).raw)
            Builtin.INT64_NE.id  -> ThirConstBool((a as ThirConstInt64).raw != (b as ThirConstInt64).raw)
            Builtin.INT64_NEG.id -> ThirConstInt64(-(a as ThirConstInt64).raw)
            Builtin.INT64_POS.id -> ThirConstInt64((a as ThirConstInt64).raw)
            Builtin.INT64_SUB.id -> ThirConstInt64((a as ThirConstInt64).raw - (b as ThirConstInt64).raw)
            Builtin.STR_EQ.id    -> ThirConstBool((a as ThirConstStr).raw == (b as ThirConstStr).raw)
            Builtin.STR_NE.id    -> ThirConstBool((a as ThirConstStr).raw != (b as ThirConstStr).raw)
            Builtin.STR_ADD.id   -> ThirConstStr((a as ThirConstStr).raw + (b as ThirConstStr).raw)
            else                 -> pushFrame { evaluate(it, symbol, parameters).outcome.valueOrNull() ?: TODO() }
        }
    }
    
    private fun evaluateWhile(frame: StackFrame, predicate: ThirValue, instructions: List<ThirInstruction>): Evaluation?
    {
        while ((evaluateExpression(frame, predicate) as ThirConstBool).raw)
            evaluateInstructions(frame, instructions).also { if (it.`return`) return it }
        return null
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
