package com.github.derg.transpiler.experimental.resolver.workers

import com.github.derg.transpiler.experimental.resolver.*
import com.github.derg.transpiler.utils.*
import java.util.*

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
     * The unique id for this worker. This id represents the worker instance, and must be unique across all workers
     * which have ever been spawned.
     */
    val id: UUID
    
    /**
     * Takes one step of the processing of the worker. If the worker is finished, it will return the output node as the
     * success value. Otherwise, whether the worker encountered a compilation failure or some dependency ordering error,
     * its current state is reported as a failure value.
     *
     * @param context The current state of the compilation process, holding all relevant information for the workers.
     */
    fun process(context: Context): Result<Phase, Outcome>
}

/**
 * A basic worker class for performing the common operations which are desired for workers. This worker can be used for
 * a great number of simplistic workers, which all follow the same uniform pattern.
 *
 * @param id The id of the worker. This value must be unique across all possible workers.
 */
abstract class WorkerBase(override val id: UUID) : Worker
{
    private var generatedData = false
    private var generatedConstraints = false
    private var generatedWorkers = false
    
    final override fun process(context: Context): Result<Phase, Outcome>
    {
        if (!generatedWorkers)
        {
            generatedWorkers = true
            val workers = generateWorkers()
            if (workers.isNotEmpty())
                return Phase.Spawn(workers).toSuccess()
        }
        
        if (!generatedConstraints)
        {
            generatedConstraints = true
            val constraints = generateConstraints(context).valueOr { return it.toFailure() }
            if (constraints.isNotEmpty())
                return Phase.Constrain(constraints).toSuccess()
        }
        
        if (!generatedData)
        {
            generatedData = true
            generateData(context)
        }
        
        return Phase.Done.toSuccess()
    }
    
    /**
     * Generates a series of workers which should process some internal part of the parent worker. Spawning workers is
     * useful as it enables a worker to delegate some responsibility to an orchestration layer instead of inlining all
     * resolution in a single function.
     *
     * Once workers have been generated, no additional workers will be generated afterwards. This method will only be
     * invoked exactly once per worker. Note that if no workers should be generated, this method should return an empty
     * list.
     */
    protected abstract fun generateWorkers(): List<Worker>
    
    /**
     * Generates the series of constraints which restricts what kinds the node the worker operates on can take.
     * Constraints can be generated across multiple workers, and will be consolidated in the resolver once all workers
     * are done.
     *
     * Once constraints have been generated, no additional constraints will be generated afterwards. This method will
     * only be invoked exactly once per worker. Note that if no constraints should be generated, this method should
     * return an empty list.
     *
     * @param context The context holding all information about type resolution during the type-checking phase.
     */
    protected abstract fun generateConstraints(context: Context): Result<List<Constraint>, Outcome>
    
    /**
     * Generates and writes relevant data to the provided [context]. The worker is free to write what information it
     * deems necessary to make headway on type-checking.
     */
    protected abstract fun generateData(context: Context)
}

///**
// * The worker takes in a list of inputs, and processes them all sequentially until no more work remains to be done. Every element
// * in the input list is worked on using a fresh worker instantiated from the worker factory. Once all inputs have been processed,
// * the worker sequence is considered to be done.
// *
// * If any worker in the chain encounters any blockers, dependencies, or other concerns, the worker sequence will halt until the
// * underlying issue has been resolved.
// *
// * @param inputs The collection of data which should be processed by the worker.
// * @param factory The way the worker is instantiated per data entry in the inputs.
// */
//class WorkerSequence<Input>(inputs: List<Input>, factory: (Input) -> Worker) : Worker
//{
//    private val workers = inputs.map { factory(it) }.asReversed().toMutableList()
//
//    override fun process(context: Context): Result<Phase, Outcome>
//    {
//        while (workers.isNotEmpty())
//        {
//            val worker = workers.last()
//            val phase = worker.process(context).valueOr { return it.toFailure() }
//
//            when (phase)
//            {
//                Phase.Done -> workers.removeLast()
//                else       -> return phase.toSuccess()
//            }
//        }
//
//        return Phase.Done.toSuccess()
//    }
//}
