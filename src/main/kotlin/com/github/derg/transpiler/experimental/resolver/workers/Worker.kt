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
     * Generates a series of workers which should process some internal part of the parent worker. Spawning workers is
     * useful as it enables a worker to delegate some responsibility to an orchestration layer instead of inlining all
     * resolution in a single function.
     *
     * These spawned workers are intended on solving sub-parts of the program stemming from the parent worker. Examples
     * include resolving the parameter list in a function definition, statements in a method, fields in a structure, and
     * so on.
     */
    fun generateWorkers(): List<Worker> = emptyList()
    
    /**
     * Generates the declaration relevant for this worker. This should only be the case for any worker operating on a
     * symbol node, such as constants, functions, structures, and so on. Workers operating on nodes such as statements
     * or expressions should not need to write and declarations.
     *
     * Note that all declarations must be generated up-front. They are necessary during name resolution, and must exist
     * when processing workers.
     */
    fun generateDeclaration(): ThirDeclaration? = null
    
    /**
     * Generates the series of constraints which restricts what kinds the node the worker operates on can take.
     * Constraints can be generated across multiple workers, and will be consolidated in the resolver once all workers
     * are done.
     *
     * @param context The context holding all information about type resolution during the type-checking phase.
     */
    fun generateConstraints(context: Context): Result<List<Constraint>, Outcome> = Success(emptyList())
    
    /**
     * Takes one step of the processing of the worker. If the worker is finished, it will return the output node as the
     * success value. Otherwise, whether the worker encountered a compilation failure or some dependency ordering error,
     * its current state is reported as a failure value.
     *
     * @param context The current state of the compilation process, holding all relevant information for the workers.
     */
    fun process(context: Context): Result<Phase, Outcome> = Phase.Done.toSuccess()
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
