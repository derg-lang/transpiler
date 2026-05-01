package com.github.derg.transpiler.experimental.real_solver

import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * Performs a type-checking step on the source code provided. This function should be invoked exactly once for every
 * module in a project, in the correct order.
 *
 * @param module The actual module which should be type-checked.
 * @param env The environment which should be updated with all type-checked information.
 * @param pkg The scope containing all information available in the package the module is part of.
 */
fun solve(
    module: HirDeclaration.ModuleDecl,
    env: Environment,
    pkg: Scope,
): Result<Solution, SolveError>
{
    val scoping = scope(pkg, module)
    val ordering = order(module)
    val solver = Solver(env, pkg, scoping, ordering)
    
    return solver.solve().mapValue { solver.solution }
}

/**
 * The collection of errors which may occur during the type-checking phase.
 */
sealed interface SolveError
{
    /**
     * The program could not be type-checked due to a cycle in dependencies. This cycle is caused by the compiler not
     * being able to deduce the order in which all symbols should be solved.
     */
    data class CompileTimeCycle(val candidates: Map<UUID, Set<UUID>>) : SolveError
}

/**
 * The solver is responsible for converting the untyped source code into a properly typed representation. The solver
 * takes an [environment] which will be updated continuously as the solver works. The environment will contain the
 * type-checked code is it is available.
 *
 * In order to perform type-checking efficiently, the solver is provided a [scoping] structure. This structure details
 * which scope is associated with every single source code element. The [ordering] structure ensures that every element
 * in the source code is visited in the appropriate order.
 */
private class Solver(
    private val environment: Environment,
    private val scope: Scope,
    private val scoping: Scoping,
    private val ordering: Ordering,
)
{
    val solution = Solution(null)
    
    /**
     * Performs work on the whole input data. The solver will work on the code it has been provided until all elements
     * are either resolved, or until no more elements can be solved. In the case the solver was successful and the
     * program is well-typed, this method returns a success.
     */
    fun solve(): Result<Unit, SolveError>
    {
        // We need to find and solve all elements which are needed at compile time. This loop takes care of the initial
        // looping to ensure all compile-time code can be executed during type-checking.
        while (true)
        {
            when (val batch = ordering.fetch())
            {
                is Batch.Empty   -> return Unit.toSuccess()
                is Batch.Invalid -> return SolveError.CompileTimeCycle(batch.candidates).toFailure()
                is Batch.Valid   -> batch.candidates.mapUntilError(::process).onFailure { return it.toFailure() }
            }
        }
    }
    
    private fun process(elementId: UUID): Result<Unit, SolveError>
    {
        TODO()
    }
}
