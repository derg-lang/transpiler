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
    
    /**
     *
     */
    fun solve(node: HirDeclaration.ModuleDecl): Result<Unit, Outcome>
    {
        val workers = mutableMapOf<UUID, Worker>()
        
        WorkerModule(scope, node).also { workers[it.id] = it }
        
        while (workers.isNotEmpty())
        {
            val worker = workers.values.firstOrNull { blockedBy[it.id]?.isEmpty() ?: true }
                ?: return Outcome.Placeholder("Cannot find any worker without dependencies - cycle detected").toFailure()
            
            val phase = worker.process(context).valueOr { return it.toFailure() }
            when (phase)
            {
                is Phase.Spawn     -> workers += phase.workers.associateBy { it.id }
                is Phase.Done      -> workers.remove(worker.id).also { markComplete(worker.id) }
                is Phase.BlockedBy -> markBlocked(worker.id, phase.workerIds)
                is Phase.Constrain -> context.environment.solve(phase.constraints)
            }
        }
        
        // Once all type-checking is completed, there should not be a single instance of an unsolved type in the entire
        // environment.
        return context.environment.validate()
    }
    
    /**
     * Marks the given [workerId] as done with its processing. This indicates a worker has finished its workload,
     * potentially unblocking other workers.
     */
    private fun markComplete(workerId: UUID)
    {
        unblocks[workerId]?.forEach { blockedBy[it]?.remove(workerId) }
        unblocks.remove(workerId)
        blockedBy.remove(workerId)
    }
    
    /**
     * Marks the given [workerId] as blocked by all the provided [blockers]. A worker which is blocked by anything, will
     * not continue its processing until all blockers have been cleared.
     */
    private fun markBlocked(workerId: UUID, blockers: Set<UUID>)
    {
        blockedBy.getOrPut(workerId) { mutableSetOf() } += blockers
        blockers.forEach { unblocks.getOrPut(it) { mutableSetOf() } += workerId }
    }
}
