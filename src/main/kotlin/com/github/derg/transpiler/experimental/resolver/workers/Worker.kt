package com.github.derg.transpiler.experimental.resolver.workers

import com.github.derg.transpiler.experimental.resolver.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * The different phases represents how far along the processing the symbol worked on proceeded. Certain symbols may have
 * dependencies which prevents it from being fully resolved in one go - this implies that every symbol goes through
 * multiple phases before they are fully resolved.
 */
sealed interface Phase
{
    /**
     * Requires that the given [nodeId] is fully type-checked before this worker can proceed anymore with its processing. This
     * typically occurs when type information within a worker is not available at compile-time.
     */
    data class DependsOn(val nodeId: UUID) : Phase
    
    /**
     * Indicates that the worker has detected one or more [constraints] on the [type] involved in its inner working. These
     * constraints are used to ensure the program  is well-typed at the end of the process; one all type constraints have been
     * solved successfully, the program is well-typed and valid.
     */
    data class Constrain(val type: ThirType, val constraints: List<Constraint>) : Phase
    
    /**
     * Instantiates any number of [workers] which are required to fully solve the parent worker. These workers are all assumed to
     * be independent of the parent worker, and can be solved in parallel. Any cross-worker dependencies must be resolved through
     * a dependency phase.
     *
     * These spawned workers are intended on solving sub-parts of the program stemming from the parent worker. Examples include
     * resolving the parameter list in a function definition, statements in a method, fields in a structure, and so on.
     */
    data class Spawn(val workers: List<Worker>) : Phase
    
    /**
     * Indicates that the worker has completely finished all its tasks, and has nothing more to do. The output of the worker has
     * been made available in the relevant data structures - the worker should be discarded and no longer used for any
     * processing.
     */
    data object Done : Phase
}

/**
 * The failure mode which occurred during the processing. Certain symbols cannot be processed for any reason. All
 * possible failure modes are encapsulated by the outcome.
 */
sealed interface Outcome
{
    /**
     * A temporary data structure used to simply gather information. We use this placeholder as a way to pass error
     * cases out from the workers, before we know what the error cases actually are.
     */
    data class Placeholder(val message: String) : Outcome
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
 */
interface Worker
{
    /**
     * Takes one step of the processing of the worker. If the worker is finished, it will return the output node as the success
     * value. Otherwise, whether the worker encountered a compilation failure or some dependency ordering error, its current
     * state is reported as a failure value.
     *
     * @param context The current state of the compilation process, holding all relevant information for the workers.
     */
    fun process(context: Context): Result<Phase, Outcome>
}

/**
 * The worker takes in a list of inputs, and processes them all sequentially until no more work remains to be done. Every element
 * in the input list is worked on using a fresh worker instantiated from the worker factory. Once all inputs have been processed,
 * the worker sequence is considered to be done.
 *
 * If any worker in the chain encounters any blockers, dependencies, or other concerns, the worker sequence will halt until the
 * underlying issue has been resolved.
 *
 * @param inputs The collection of data which should be processed by the worker.
 * @param factory The way the worker is instantiated per data entry in the inputs.
 */
class WorkerSequence<Input>(inputs: List<Input>, factory: (Input) -> Worker) : Worker
{
    private val workers = inputs.map { factory(it) }.asReversed().toMutableList()
    
    override fun process(context: Context): Result<Phase, Outcome>
    {
        while (workers.isNotEmpty())
        {
            val worker = workers.last()
            val phase = worker.process(context).valueOr { return it.toFailure() }
            
            when (phase)
            {
                Phase.Done -> workers.removeLast()
                else       -> return phase.toSuccess()
            }
        }
        
        return Phase.Done.toSuccess()
    }
}
