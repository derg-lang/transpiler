package com.github.derg.transpiler.experimental.resolver

import com.github.derg.transpiler.experimental.resolver.workers.*
import java.util.*

/**
 * The different phases represents how far along the processing the symbol worked on proceeded. Certain symbols may have
 * dependencies which prevents it from being fully resolved in one go - this implies that every symbol goes through
 * multiple phases before they are fully resolved.
 */
sealed interface Phase
{
    /**
     * Requires that the given [workerIds] are completed before this worker can proceed anymore with its processing.
     * This typically is the case when type information within a worker is not available at compile-time. When a worker
     * require that a specific expression is type-checked, that specific worker can be marked as a blocker.
     *
     * Once an evaluation request has been provided, the worker providing this value will no longer receive any
     * processing. This is an effective way to ensure that a piece of code cannot be type-checked before all code
     * dependencies have been fully resolved.
     *
     * Blockers can be provided as many times as necessary from a worker. There are no limitations on how frequently
     * this can happen, although blocking a worker may lead to deadlocks where no worker may proceed. This can happen in
     * particularly circular and hairy type situations.
     */
    data class BlockedBy(val workerIds: Set<UUID>) : Phase
    
    /**
     * Indicates that the worker has detected one or more [constraints] involved in its inner working. These constraints
     * are used to ensure the program  is well-typed at the end of the process; once all type constraints have been
     * solved successfully, the program is well-typed and valid.
     */
    data class Constrain(val constraints: List<Constraint>) : Phase
    
    /**
     * Indicates that the worker has completely finished all its tasks, and has nothing more to do. The output of the
     * worker has been made available in the relevant data structures - the worker should be discarded and no longer
     * used for any processing.
     *
     * Any workers which depend on the given node's completion, are permitted to resume once this variant has been
     * provided.
     */
    data object Done : Phase
    
    /**
     * Instantiates any number of [workers] which are required to fully solve the parent worker. These workers are all
     * assumed to be independent of the parent worker, and can be solved in parallel. Any cross-worker dependencies must
     * be resolved through a dependency phase.
     *
     * These spawned workers are intended on solving sub-parts of the program stemming from the parent worker. Examples
     * include resolving the parameter list in a function definition, statements in a method, fields in a structure, and
     * so on.
     */
    data class Spawn(val workers: List<Worker>) : Phase
}
