package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.phases.resolver.workers.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * Converts the input [node] into an environment of type-checked symbols. All symbols within the environment should be
 * ready for further semantic analysis and interpretation or lowering.
 */
fun resolve(node: HirDeclaration.SegmentDecl): Result<Environment, String>
{
    val resolver = Resolver(Builtin.environment, Builtin.scope)
    return resolver.resolve(node).mapValue { Builtin.environment }
}

/**
 * The resolver is responsible for converting an arbitrary amount of code in a module into a type-checked version of the
 * same code. It works in the provided [environment], inheriting everything visible in the given [scope].
 */
internal class Resolver(private val environment: Environment, private val scope: Scope)
{
    /**
     * The dependencies associated with a specific node.
     */
    private class Dependency
    {
        val declarations = mutableSetOf<UUID>()
        val definitions = mutableSetOf<UUID>()
    }
    
    /**
     * The dependencies which must be resolved before the key node can be processed further. A dependency records the
     * order in which all nodes must be resolved, in order to perform a full type-check of the entire program. Nodes
     * which do not have any dependencies can be processed right away.
     *
     * The [dependants] is the map of which nodes depend on the key node. This indicates which nodes should be processed
     * once the key node has been declared and/or defined.
     */
    private val dependencies = mutableMapOf<UUID, Dependency>()
    private val dependants = mutableMapOf<UUID, Dependency>()
    
    /**
     * The workers represent the current state of type-checking for all nodes in the program. Each worker can be paused
     * and resumed as needed, depending on whether the worker require another node to be declared and/or defined.
     */
    private val workers = mutableMapOf<UUID, Worker<Phase>>()
    
    /**
     * Resolves the information present in the given [node] into a type-checked version. This method returns a success
     * value when the provided code type-checked. Otherwise, an error indicating the problem in the source code is
     * returned.
     */
    fun resolve(node: HirDeclaration.SegmentDecl): Result<Unit, String>
    {
        registerNewWorker(node.id, SegmentDefiner(node, environment, scope))
        
        while (dependencies.isNotEmpty())
            takeOneStep().onFailure { return it.toFailure() }
        return Unit.toSuccess()
    }
    
    private fun takeOneStep(): Result<Unit, String>
    {
        // All nodes which have no dependencies on other nodes can be processed. Nodes which have other dependencies,
        // must wait until all their dependencies have been sorted out.
        val candidates = dependencies.filter { it.value.declarations.isEmpty() }
        if (candidates.isEmpty())
            return "Found no node without any dependency! This might indicate a recursive issue in the type checker".toFailure()
        
        for (nodeId in candidates.keys)
        {
            val worker = workers[nodeId] ?: throw IllegalStateException("The node '$nodeId' has no worker")
            val outcome = worker.process()
            
            when (outcome)
            {
                is Success -> when (outcome.value)
                {
                    is Phase.Spawn    -> outcome.value.workers.forEach { registerNewWorker(it.key, it.value) }
                    is Phase.Declared -> Unit // TODO: Figure out what to do with this one.
                    is Phase.Defined  -> recordDeclared(nodeId)
                }
                is Failure -> when (outcome.error)
                {
                    is Outcome.RequireDeclaration -> recordDependency(nodeId, outcome.error.ids)
                    is Outcome.RequireDefinition  -> recordDependency(nodeId, outcome.error.ids)
                    else                          -> return outcome.error.toString().toFailure()
                }
            }
        }
        
        return Unit.toSuccess()
    }
    
    private fun registerNewWorker(nodeId: UUID, worker: Worker<Phase>)
    {
        workers[nodeId] = worker
        dependencies[nodeId] = Dependency()
        dependants[nodeId] = Dependency()
    }
    
    private fun recordDependency(nodeId: UUID, dependsOn: Set<UUID>)
    {
        dependencies[nodeId]!!.declarations += dependsOn
        dependsOn.forEach { dependants[it]!!.declarations += nodeId }
    }
    
    private fun recordDeclared(nodeId: UUID)
    {
        dependants[nodeId]!!.declarations.forEach { dependencies[it]!!.declarations -= nodeId }
        dependants.remove(nodeId)
        dependencies.remove(nodeId)
        workers.remove(nodeId)
    }
}
