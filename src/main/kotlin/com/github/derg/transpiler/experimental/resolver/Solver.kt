package com.github.derg.transpiler.experimental.resolver

import com.github.derg.transpiler.experimental.resolver.workers.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * The solver is the component responsible for type-checking single units of the program at a time. It will be running
 * type inference on the code, verifying that all types matches the user-provided types where applicable. All scoping
 * and flow typing rules will be handled by the solver as well.
 *
 * Note that the solver can be re-used between different modules. All information present in the context and scope will
 * be modified by the solver as it works on modules. Do note that modules must be processed in the appropriate order,
 * determined by how modules depends on each other.
 *
 * @param context The context which should be populated by the solver.
 * @param scope The package scope the solver should operate in.
 */
class Solver(
    private val context: Context,
    private val scope: Scope,
)
{
    // Workers which are blocked due to a need for evaluation, are recorded here. The blocked worker ids are the keys,
    // and the values are the workers which they are blocked by. The unblocks map is the reverse - it stores a worker id
    // as its keys, and the nodes which are unblocked as the values.
    private val blockedBy = mutableMapOf<UUID, MutableSet<UUID>>()
    private val unblocks = mutableMapOf<UUID, MutableSet<UUID>>()
    private val completed = mutableSetOf<UUID>()
    
    /**
     * Generates all relevant information and performs a type-checking of the given [node]. All modules must be
     * type-checked in the order determined by the module's dependency graph - modules which have no dependencies must
     * be solved first, followed by the modules depending on only solved modules.
     *
     * A module can only be fully type-checked if all its dependencies have already been type-checked successfully, and
     * if everything in the module is well-typed. This means that all scope resolution, types, mutability, and so on
     * must be valid. If the solver encounters any issues during the type-checking, the relevant error is returned.
     */
    fun solve(node: HirDeclaration.ModuleDecl): Result<Unit, Outcome>
    {
        val workers = generateWorkers(node)
        
        // The order in which operations are performed makes a difference. We need to ensure that all declarations are
        // created in the first pass, such that we can find them later by their ids when necessary.
        // TODO: Do we really need to generate these declarations ahead of time? Can we not use the HIR data instead?
        workers
            .mapNotNull { it.generateDeclaration() }
            .onEach { context.declarations[it.id] = it }
        
        // A large number of constraints may be generated immediately; these constraints form the majority of normal
        // type-checking. We still need to find a decent number of type information, but it must be generated in later
        // passes since those constraints require compile-time code execution.
        workers
            .mapUntilError { it.generateConstraints(context) }
            .valueOr { return it.toFailure() }
            .flatten()
            .let { context.environment.solve(it) }
            .valueOr { return it.toFailure() }
        
        // With the foundation in place, we are now able to perform all the hard work! We can run through all workers,
        // processing them in the order required to solve the type-checking part. This is where all compile-time code
        // execution, registering of new constraints, and other shenanigans takes place.
        processWorkers(workers)
            .valueOr { return it.toFailure() }
        
        // Once all type-checking is completed, there should not be a single instance of an unsolved type in the entire
        // environment.
        return context.environment.validate()
    }
    
    /**
     * Generates the collection of workers which are required to fully resolve all source code in the given [node]. Each
     * worker is responsible for resolving a small part of the program, and holds the state on each individual element
     * encountered.
     */
    private fun generateWorkers(node: HirDeclaration.ModuleDecl): List<Worker>
    {
        val ready = mutableListOf<Worker>()
        var new = listOf<Worker>(WorkerModule(scope, node))
        while (new.isNotEmpty())
        {
            ready += new
            new = new.flatMap { it.generateWorkers() }
        }
        return ready
    }
    
    /**
     * Runs the [workers] until they have all completed, or until an error preventing any further type-checking is
     * encountered. Workers will be processed in whichever order is required to pass type-checking; the order is
     * determined by cross-worker dependencies.
     */
    private fun processWorkers(workers: List<Worker>): Result<Unit, Outcome>
    {
        val remaining = workers.associateBy { it.id }.toMutableMap()
        while (remaining.isNotEmpty())
        {
            val worker = remaining.values.firstOrNull { blockedBy[it.id]?.isEmpty() ?: true }
                ?: return Outcome.Placeholder("Cannot find any worker without dependencies - cycle detected").toFailure()
            
            val phase = worker.process(context).valueOr { return it.toFailure() }
            when (phase)
            {
                is Phase.Spawn     -> remaining += phase.workers.associateBy { it.id }
                is Phase.Done      -> finishWorker(worker).also { remaining -= worker.id }
                is Phase.BlockedBy -> blockWorker(worker, phase.workerIds)
                is Phase.Constrain -> context.environment.solve(phase.constraints).valueOr { return it.toFailure() }
            }
        }
        
        return Unit.toSuccess()
    }
    
    /**
     * Marks the given [worker] as done with its processing. This indicates a worker has finished its workload,
     * potentially unblocking other workers.
     */
    private fun finishWorker(worker: Worker)
    {
        unblocks[worker.id]?.forEach { blockedBy[it]?.remove(worker.id) }
        unblocks -= worker.id
        blockedBy -= worker.id
        completed += worker.id
    }
    
    /**
     * Marks the given [worker] as blocked by all the provided [blockers]. A worker which is blocked by anything, will
     * not continue its processing until all blockers have been cleared.
     */
    private fun blockWorker(worker: Worker, blockers: Set<UUID>)
    {
        val unsolved = blockers - completed
        blockedBy.getOrPut(worker.id) { mutableSetOf() } += unsolved
        unsolved.forEach { unblocks.getOrPut(it) { mutableSetOf() } += worker.id }
    }
}
