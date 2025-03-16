package com.github.derg.transpiler.source.thir

import com.github.derg.transpiler.source.*
import java.util.*

/**
 * Objects which may be identified in source code are known as symbols. The symbols are typically located within a scope
 * and may be resolved based on name or id.
 */
sealed interface ThirSymbol
{
    val id: UUID
    val name: String
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Layout
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * The collection of a logical group of modules is known as a package, which consists of any number of source files
 * joined together in an arbitrary manner. The modules within the package are permitted circular dependencies, under the
 * condition that all symbols may be resolved in an unambiguous manner.
 */
data class ThirPackage(
    override val id: UUID,
    override val name: String,
    
    // Symbols present within the object
    val modulesIds: Set<UUID>,
) : ThirSymbol

/**
 * The module represents a
 */
data class ThirModule(
    override val id: UUID,
    override val name: String,
    
    // Symbols present within the object
    val segmentIds: Set<UUID>,
) : ThirSymbol

/**
 * The segment represents a collection of symbols which are logically grouped into a cohesive unit. Every segment may
 * contain any number of functions, variables, types, and so on. Segments are not independent of each other, but have
 * their own scope.
 */
data class ThirSegment(
    override val id: UUID,
    override val name: String,
    
    // Symbols present within the object
    val structIds: Set<UUID>,
    val conceptIds: Set<UUID>,
    val constantIds: Set<UUID>,
    val functionIds: Set<UUID>,
) : ThirSymbol

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Concepts represents a contract a type must satisfy. It does not provide any functionality or data.
 */
data class ThirConcept(
    override val id: UUID,
    override val name: String,
    val visibility: Visibility,
    
    // Symbols present within the object
    val fieldIds: Set<UUID>,
    val genericIds: Set<UUID>,
    val functionIds: Set<UUID>,
) : ThirSymbol

/**
 * Executable code is found within functions, which form a smaller executable part of the program. Functions may take
 * any number of input parameters, and may terminate processing by returning nothing, returning a value, or by raising
 * and error.
 */
data class ThirFunction(
    override val id: UUID,
    override val name: String,
    val type: ThirType.Function,
    val visibility: Visibility,
    val instructions: List<ThirInstruction>,
    
    // Symbols present within the object
    val genericIds: List<UUID>,
    val variableIds: List<UUID>,
    val parameterIds: List<UUID>,
) : ThirSymbol

/**
 * All data within a program must be represented as a type. Every type requires some amount of physical space in memory,
 * which is used to allocate instances of the type on the heap or the stack. Types may be instantiated as variables or
 * parameters, which may be accessed or modified as needed.
 */
data class ThirStruct(
    override val id: UUID,
    override val name: String,
    val visibility: Visibility,
    
    // Symbols present within the object
    val fieldIds: Set<UUID>,
    val methodIds: Set<UUID>,
) : ThirSymbol

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Values
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Constants are values which do not exist on either the stack, nor the heap. They are compile-time values, which will
 * never mutate nor change. They cannot be re-assigned or modified in any way, shape, or form. They can always be
 * inlined into code by the compiler.
 */
data class ThirConstant(
    override val id: UUID,
    override val name: String,
    val type: ThirType,
    val value: ThirValue,
    val visibility: Visibility,
) : ThirSymbol

/**
 * Properties holds values within a data structure of various types. Properties may hold aa variety of different types
 * of data, such as raw values, references to other variables or functions, locations in memory, and so on.
 */
data class ThirField(
    override val id: UUID,
    override val name: String,
    val type: ThirType,
    val value: ThirValue?,
    val visibility: Visibility,
    val assignability: Assignability,
) : ThirSymbol

/**
 * Some functions may require additional input in order to adequately perform their functionality. This additional input
 * is modeled as parameters. A parameter is different from a variable as parameters are never stored in main memory, but
 * are rather always located on the stack.
 */
data class ThirParameter(
    override val id: UUID,
    override val name: String,
    val type: ThirType,
    val value: ThirValue?,
    val passability: Passability,
) : ThirSymbol

/**
 * Variables holds values stored in memory at some arbitrary location. Variables may hold aa variety of different types
 * of data, such as raw values, references to other variables or functions, locations in memory, and so on. Variables
 * may be located either on the stack, or in the heap.
 */
data class ThirVariable(
    override val id: UUID,
    override val name: String,
    val type: ThirType,
    val assignability: Assignability,
) : ThirSymbol
