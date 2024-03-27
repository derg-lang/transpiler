package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import java.util.*

/**
 *
 */
sealed interface HirSymbol
{
    val id: UUID
    val name: String
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Structures
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Packages represents an entire program or library, containing all relevant code and constructs to make it work.
 */
data class HirPackage(
    override val id: UUID,
    override val name: String,
    
    // Symbols present within the object
    val modules: List<HirModule>,
) : HirSymbol

/**
 * Modules are high-level constructs which contains any number of identifiable objects. Modules may import any number of
 * other modules, forming the dependency graph between them.
 */
data class HirModule(
    override val id: UUID,
    override val name: String,
    
    // Symbols present within the object
    val segments: List<HirSegment>,
) : HirSymbol

/**
 * A segment represents a single source file of code.
 */
data class HirSegment(
    override val id: UUID,
    override val name: String,
    val imports: Set<String>,
    
    // Symbols present within the object
    val structs: List<HirStruct>,
    val concepts: List<HirConcept>,
    val constants: List<HirConstant>,
    val functions: List<HirFunction>,
) : HirSymbol

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Concepts represents a contract a type must satisfy. It does not provide any functionality or data.
 */
data class HirConcept(
    override val id: UUID,
    override val name: String,
    val visibility: Visibility,
    
    // Symbols present within the object
    val fields: List<HirField>,
    val generics: List<HirGeneric>,
    val functions: List<HirFunction>,
) : HirSymbol

/**
 * Functions are callable subroutines which allows a program to be structured into smaller segments.
 */
data class HirFunction(
    override val id: UUID,
    override val name: String,
    val type: HirTypeCall,
    val visibility: Visibility,
    val instructions: List<HirInstruction>,
    
    // Symbols present within the object
    val generics: List<HirGeneric>,
    val variables: List<HirVariable>,
    val parameters: List<HirParameter>,
) : HirSymbol

/**
 * Methods are callable subroutines which operate on a specific type, allowing programs to be decomposed into smaller
 * segments.
 */
data class HirMethod(
    override val id: UUID,
    override val name: String,
    val type: HirTypeCall,
    val visibility: Visibility,
    val instructions: List<HirInstruction>,
    
    // Symbols present within the object
    val generics: List<HirGeneric>,
    val variables: List<HirVariable>,
    val parameters: List<HirParameter>,
) : HirSymbol

/**
 * Literals are special data conversion functions, which allows the developer to convert raw numbers and text literals
 * into ordinary types.
 */
data class HirLiteral(
    override val id: UUID,
    override val name: String,
    val type: HirTypeLiteral,
    val visibility: Visibility,
    val instructions: List<HirInstruction>,
    
    // Symbols present within the object
    val variables: List<HirVariable>,
    val parameter: HirParameter,
) : HirSymbol

/**
 * Data structures represents a collection of properties. It does not provide any additional functionality.
 */
data class HirStruct(
    override val id: UUID,
    override val name: String,
    val visibility: Visibility,
    
    // Symbols present within the object
    val fields: List<HirField>,
    val methods: List<HirMethod>,
    val generics: List<HirGeneric>,
) : HirSymbol

/**
 * Closures are anonymous callable subroutines which allows values from the surroundings to be captured.
 */
// TODO: This may need to be represented as a synthetic type instead; closures cannot be named in the general case. By
//       representing it as a construct of its own like this, there are likely many edge cases to consider. It is likely
//       more sensible to implement closures via unnamed data structures containing all captured values, overloaded as
//       callable.
//data class HirClosure(
//    val id: UUID,
//    val parameters: List<HirParameter>,
//    val instructions: List<HirInstruction>,
//    val error: HirType?,
//    val output: HirType?,
//)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Values
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Constants represents a value or attribute associated with a module. They cannot be re-assigned or mutated in any way,
 * shape, or form.
 */
data class HirConstant(
    override val id: UUID,
    override val name: String,
    val type: HirType,
    val value: HirValue,
    val visibility: Visibility,
) : HirSymbol

/**
 * Fields represents a value or attribute associated with an instance of a type.
 */
data class HirField(
    override val id: UUID,
    override val name: String,
    val type: HirType,
    val value: HirValue?,
    val visibility: Visibility,
    val assignability: Assignability,
) : HirSymbol

/**
 * Type parameters represents a generic type used in a construct. These parameters are used to generalize across a whole
 * range of types, rather than just a single type. Generics may be constrained by any number of [concepts].
 */
data class HirGeneric(
    override val id: UUID,
    override val name: String,
    val type: HirType?,
    val value: HirValue?,
    val concepts: List<String>,
) : HirSymbol

/**
 * The parameter represents a specific input expected to a callable construct.
 */
data class HirParameter(
    override val id: UUID,
    override val name: String,
    val type: HirType,
    val value: HirValue?,
    val passability: Passability,
) : HirSymbol

/**
 * Variables represents a value which exists exclusively on the stack, and can only be defined within a callable body.
 */
data class HirVariable(
    override val id: UUID,
    override val name: String,
    val type: HirType?,
    val value: HirValue,
    val assignability: Assignability,
) : HirSymbol
