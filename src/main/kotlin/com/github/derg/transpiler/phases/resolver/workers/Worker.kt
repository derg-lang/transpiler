package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.math.*
import java.util.*

/**
 * When a worker performs its operations, sometimes it must make changes to the environment surrounding it. This may
 * include spawning additional workers to resolve, new dependencies, and so on. The outcome captures the state of the
 * workers and handles type inference accordingly.
 */
sealed interface Outcome
{
    //////////////////////////////////////////////////////////
    // Various state-based outcomes - these are recoverable //
    //////////////////////////////////////////////////////////
    
    /**
     * The symbols with the given [ids] must be resolved before the worker can continue its operation.
     */
    data class RequireDeclaration(val ids: Set<UUID>) : Outcome
    
    /**
     * The symbols with the given [ids] must be defined before the worker can continue its operation.
     */
    data class RequireDefinition(val ids: Set<UUID>) : Outcome
    
    ////////////////////////////////////////////////////
    // Hard errors, all these should fail compilation //
    ////////////////////////////////////////////////////
    
    // Call-related errors.
    
    /**
     * A function call could not be resolved due to missing the parameter with the given [name].
     */
    data class MissingParameter(val name: String) : Outcome
    
    /**
     * An unexpected parameter with the given [name] and [value] was provided as a function argument.
     */
    data class UnexpectedParameter(val name: String?, val value: HirExpression) : Outcome
    
    /**
     * A function call could not be resolved due to a mismatched type conflict. The function [expected] a certain type,
     * but [received] a different one.
     */
    data class MismatchedType(val expected: ThirType, val received: ThirType) : Outcome
    
    /**
     * A symbol with the given [name] was attempted invoked. No overloads of the name compatible with the parameter
     * list were found. The candidates which were found, were all rejected due to the provided [errors].
     */
    data class NoOverloadAvailable(val name: String, val errors: List<Outcome>) : Outcome
    
    // Identifier-related errors.
    
    /**
     * The identifier with the given [name] is unrecognized.
     */
    data class UnknownIdentifier(val name: String) : Outcome
    
    /**
     * The identifier with the given [name] is overloaded with multiple meanings, and could not be resolved.
     */
    data class OverloadedIdentifier(val name: String) : Outcome
    
    // Literal-related errors.
    
    /**
     * The user-provided [value] does not fit inside the signed integer it was attempted to be shoe-horned into.
     */
    data class InvalidInteger(val value: BigInteger) : Outcome
    
    /**
     * The user-provided [value] does not fit inside the floating point number it was attempted to be shoe-horned into.
     */
    data class InvalidDecimal(val value: BigDecimal) : Outcome
    
    // Other miscellaneous errors.
    
    /**
     * An error case which exists in code, but has not yet been handled properly. These errors indicate some other error
     * condition, but it has not been categorized and sorted into a proper type.
     */
    @Deprecated("Avoid marking errors as unhandled, and rather implement the handling fully, you silly derg!")
    data class Unhandled(val message: String) : Outcome
    
    /**
     * A feature has been made use of, which is not yet supported in the compiler. These errors must be phased out over
     * time, ensuring that the user can make use of all language features offered.
     */
    @Deprecated("Avoid marking features as unsupported, and rather implement the feature fully, you silly derg!")
    data class Unsupported(val message: String) : Outcome
}

/**
 * The worker represents a construction process for declaring and defining a symbol, or some other part of the source
 * code. The input to the worker is the current environment, plus a high intermediate representation state of the source
 * code to be processed. Once all prerequisites for the node to be processed are met, the node can be turned into the
 * corresponding typed high intermediate representation.
 *
 * Each worker must track the state of their own progress as they work. Workers are not required to complete their work
 * in a single cycle, and may instead require another worker to complete first. The ordering of all workers may change
 * as the source code is processed - sometimes different nodes depends on each other's declarations, but cannot be
 * defined until both are declared.
 *
 * All workers should take in an [Environment] and [Scope], which indicates what currently exists and what the worker
 * can access.
 */
interface Worker<Output>
{
    /**
     * Attempts to resolve the input node to the [Output] type. If the worker is finished, it will return the output
     * node as the success value. Otherwise, whether the worker encountered a compilation failure or some dependency
     * ordering error, its current state is reported as a failure value.
     */
    fun process(): Result<Output, Outcome>
}

/**
 * The list worker is a utility component which can work on a collection of input nodes. The worker converts from an
 * [Input] type to the [Output] type, only returning when all nodes have been successfully operated on. The order of all
 * outputs will be the same as the order of all inputs.
 */
class WorkerList<Input, Output>(inputs: List<Input>, factory: (Input) -> Worker<Output>) : Worker<List<Output>>
{
    private val workers = inputs.map { factory(it) }.toMutableList()
    private val outputs = mutableListOf<Output>()
    
    override fun process(): Result<List<Output>, Outcome>
    {
        while (workers.isNotEmpty())
        {
            val worker = workers.first()
            val output = worker.process().valueOr { return it.toFailure() }
            
            workers.removeAt(0)
            outputs += output
        }
        
        return outputs.toSuccess()
    }
}
