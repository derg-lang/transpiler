package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.thir.*
import java.util.*

/**
 * All tables which are relevant for full type resolution. Each table is populated during type resolution, and is only
 * guaranteed to be fully constructed once the type resolution phase is fully completed. During type resolution, the
 * tables may be incomplete, not containing all information about every symbol.
 *
 * @param types The type information of all symbols which has been resolved during type resolution.
 * @param symbols The actual symbols which have been fully constructed during type resolution.
 */
data class Tables(val types: TypeTable, val symbols: SymbolTable)

/**
 * The type table contains information about the type of all symbols which have been encountered so far during type
 * resolution. Once type resolution is fully complete, all symbols declared within the source code will have a proper
 * entry in this table.
 */
class TypeTable
{
    // Binding types.
    val fields = mutableMapOf<UUID, ThirType>()
    val parameters = mutableMapOf<UUID, ThirType>()
    val variables = mutableMapOf<UUID, ThirType>()
    
    // Call types.
    val functions = mutableMapOf<UUID, ThirType.Call>()
    
    // Data types.
    val structs = mutableMapOf<UUID, Unit>()
}

/**
 * The symbol table holds a reference to all symbols which have been fully type-checked and is considered well-formed.
 * Every symbol which is fully type-checked and valid will be inserted into the table.
 */
class SymbolTable
{
    // Binding types.
    val fields = mutableMapOf<UUID, ThirField>()
    val parameters = mutableMapOf<UUID, ThirParameter>()
    val variables = mutableMapOf<UUID, ThirVariable>()
    
    // Call types.
    val functions = mutableMapOf<UUID, ThirFunction>()
    
    // Data types.
    val structs = mutableMapOf<UUID, ThirStruct>()
}
