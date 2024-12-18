package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.thir.*
import java.util.*

/**
 * The symbol table holds a reference to all symbols which have been found and resolved. It also keeps track of the
 * types of symbols that have been found but not yet fully resolved.
 */
class SymbolTable
{
    val fields = mutableMapOf<UUID, ThirField>()
    val functions = mutableMapOf<UUID, ThirFunction>()
    val parameters = mutableMapOf<UUID, ThirParameter>()
    val structs = mutableMapOf<UUID, ThirStruct>()
    val variables = mutableMapOf<UUID, ThirVariable>()
    
    override fun toString(): String
    {
        return "SymbolTable(\n\tfields=${fields.values},\n\tfunctions=${functions.values},\n\tparameters=${parameters.values},\n\tstructs=${structs.values},\n\tvariables=${variables.values})"
    }
}

/**
 * The type table holds a reference to the type of various objects that have been found and processed. The types are
 * analyzed and made available before the full type resolution takes place.
 */
class TypeTable
{
    val fields = mutableMapOf<UUID, ThirType>()
    val functions = mutableMapOf<UUID, ThirType.Function>()
    val literals = mutableMapOf<UUID, ThirType.Function>()
    val parameters = mutableMapOf<UUID, ThirType>()
    val variables = mutableMapOf<UUID, ThirType>()
}
