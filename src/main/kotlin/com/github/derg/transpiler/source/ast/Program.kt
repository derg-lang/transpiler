package com.github.derg.transpiler.source.ast

/**
 * The source code of the entire program represented as a single abstract syntax tree.
 *
 * @param applications The collection of applications which should be built concurrently.
 * @param packages The collection of packages which together makes up the entire program.
 */
data class AstProgram(
    val applications: List<AstSegment>,
    val packages: List<AstPackage>,
)

/**
 * The core data structure holding the shape of the entire package. The abstract syntax tree contains every syntactic
 * element of the source code, stripped of all non-semantic information. Comments and whitespace has been fully stripped
 * from the source code.
 *
 * @param name The name of the package.
 * @param modules The collection of modules which makes up the package.
 */
data class AstPackage(
    val name: String,
    val modules: List<AstModule>,
)

/**
 * Any module may contain an arbitrary number of nested elements. Modules are what forms the basis of the application,
 * enabling it to be modularized.
 *
 * @param name The name of the module.
 * @param segments The source code files which reside within this module.
 */
data class AstModule(
    val name: String,
    val segments: List<AstSegment>,
)

/**
 * Every source code file is parsed into a single segment, in which the total collection of segments with the same
 * module name form a single module. The segment forms the most basic building block when structuring code, and is the
 * component which allows code fragmentation to take place.
 *
 * @param imports The modules which are to be imported into this segment.
 * @param definitions All components which are injected into the module by this segment.
 */
data class AstSegment(
    val imports: List<String>,
    val definitions: List<AstSymbol>,
)
