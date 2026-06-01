package com.github.derg.transpiler.experimental.resolver

import java.util.*

/**
 * The total overview of all information currently available to the type-checking process. The context keeps track of
 * all resources which have been created, are available, type information, and everything else which is needed to
 * complete the work.
 */
class Context
{
    /**
     * The environment is a collection of all symbols which have been created and recorded during type resolution. Every new
     * symbol encountered should be added to this collection as type-checking takes place.
     */
    val declarations = Builtin.generateDeclarations().toMutableMap()
    
    /**
     * A collection of values which have been computed during the type-checking phase. These values are computed when a
     * worker requests an evaluation to be performed before it may continue working on its task.
     */
    val evaluations = mutableMapOf<UUID, ThirExpression>()
    
    /**
     * A collection of kinds which have been computed during the type-checking phase. These types are continuously
     * updated as the workers operate on the provided data.
     */
    val environment = Builtin.generateEnvironment()
}
