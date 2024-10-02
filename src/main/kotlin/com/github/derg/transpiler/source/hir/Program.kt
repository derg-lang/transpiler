package com.github.derg.transpiler.source.hir

import java.util.*

/**
 * Programs contain all information needed to build up all applications and dependencies from source code.
 */
data class HirProgram(
    val applications: List<HirSegment>,
    val packages: List<HirPackage>,
)

/**
 * Packages represents an entire program or library, containing all relevant code and constructs to make it work.
 */
data class HirPackage(
    val id: UUID,
    val name: String,
    val modules: List<HirModule>,
)

/**
 * Modules are high-level constructs which contains any number of identifiable objects. Modules may import any number of
 * other modules, forming the dependency graph between them.
 */
data class HirModule(
    val id: UUID,
    val name: String,
    val segments: List<HirSegment>,
)

/**
 * A segment represents a single source file of code.
 */
data class HirSegment(
    val imports: Set<String>,
    val structs: List<HirStruct>,
    val concepts: List<HirConcept>,
    val constants: List<HirConstant>,
    val functions: List<HirFunction>,
)
