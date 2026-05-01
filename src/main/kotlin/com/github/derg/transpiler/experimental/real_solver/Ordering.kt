package com.github.derg.transpiler.experimental.real_solver

import com.github.derg.transpiler.source.hir.*
import java.util.*

/**
 * Generates the ordering for the provided [module]. The output structure contains the information detailing how the
 * source code in the module should be processed, to avoid issues where part of the code has not been resolved, but is
 * required to type-check something else in the code.
 *
 * It is important to note that the ordering does not depend on name resolution or any other similar aspect of the
 * source code. The ordering only ensures that code may be solved in a way which satisfies the interoperability between
 * the type-checker and the interpreter.
 */
internal fun order(module: HirDeclaration.ModuleDecl): Ordering =
    Ordering().apply { store(module) }

private fun Ordering.store(symbol: HirDeclaration)
{
    register(symbol.id)
    
    when (symbol)
    {
        is HirDeclaration.ConstantDecl      -> store(symbol.id, symbol.kind)
        is HirDeclaration.FieldDecl         -> store(symbol.id, symbol.kind)
        is HirDeclaration.ParameterDecl     -> store(symbol.id, symbol.kind)
        is HirDeclaration.TypeParameterDecl -> store(symbol.id, symbol.kind)
        is HirDeclaration.ModuleDecl        -> for (segment in symbol.segments)
        {
            segment.constants.forEach { store(it) }
            segment.constants.forEach { append(symbol.id, it.id) }
            segment.functions.forEach { store(it) }
            segment.functions.forEach { append(symbol.id, it.id) }
            segment.structures.forEach { store(it) }
            segment.structures.forEach { append(symbol.id, it.id) }
        }
        is HirDeclaration.StructureDecl     ->
        {
            symbol.ctorEntries.forEach { store(it) }
            symbol.ctorEntries.forEach { append(symbol.id, it.id) }
            symbol.fields.forEach { store(it) }
            symbol.fields.forEach { append(symbol.id, it.id) }
            symbol.methods.forEach { store(it) }
            symbol.methods.forEach { append(symbol.id, it.id) }
            symbol.typeParameters.forEach { store(it) }
            symbol.typeParameters.forEach { append(symbol.id, it.id) }
        }
        is HirDeclaration.FunctionDecl      ->
        {
            symbol.parameters.forEach { store(it) }
            symbol.parameters.forEach { append(symbol.id, it.id) }
            symbol.typeParameters.forEach { store(it) }
            symbol.typeParameters.forEach { append(symbol.id, it.id) }
            
            store(symbol.id, symbol.valueKind)
            store(symbol.id, symbol.errorKind)
        }
        is HirDeclaration.MethodDecl        ->
        {
            symbol.parameters.forEach { store(it) }
            symbol.parameters.forEach { append(symbol.id, it.id) }
            symbol.typeParameters.forEach { store(it) }
            symbol.typeParameters.forEach { append(symbol.id, it.id) }
            
            store(symbol.id, symbol.valueKind)
            store(symbol.id, symbol.errorKind)
        }
    }
}

/**
 * Marks the [elementId] as depending on whatever expression is hidden inside the [kind]. This is merely a helper
 * function for extracting all dependencies in the underlying source code. Dependencies are only recorded for the kinds
 * which take in an expression value, as these cannot be solved immediately.
 */
private fun Ordering.store(elementId: UUID, kind: HirKind?)
{
    if (kind is HirKind.Value)
        store(elementId, kind.type)
}

private fun Ordering.store(elementId: UUID, type: HirType)
{
    when (type)
    {
        is HirType.Expression -> append(elementId, type.value.id)
        is HirType.Function   ->
        {
            type.valueKind?.let { store(elementId, it) }
            type.errorKind?.let { store(elementId, it) }
            type.parameters.forEach { store(elementId, it) }
        }
    }
}

/**
 * The collection of outcomes which are possible to encounter when fetching the next batch of elements to solve from the
 * order.
 */
internal sealed interface Batch
{
    /**
     * There were no more candidates left to fetch from the ordering. Everything has been fully resolved.
     */
    data object Empty : Batch
    
    /**
     * The collection of [candidates] which are available for solving. These elements are all known to have exactly zero
     * dependencies on them.
     */
    data class Valid(val candidates: Set<UUID>) : Batch
    
    /**
     * No elements could be retrieved, since there were no elements left that had zero dependencies. All [candidates]
     * which were remaining, had at least one dependency left to solve.
     */
    data class Invalid(val candidates: Map<UUID, Set<UUID>>) : Batch
}

/**
 * The ordering data structure holds information about the order in which all nodes in a program must be evaluated in,
 * in order to properly type-check an entire section of the program. Every element in the source code must be assigned a
 * unique id, which can be used to track which element require that other elements are fully solved before moving on.
 */
internal class Ordering
{
    /**
     * A map from an element id, to the elements which must be fully resolved before it can be solved. This is the main
     * map detailing which
     */
    private val dependenciesById = mutableMapOf<UUID, MutableSet<UUID>>()
    
    /**
     * A map from an element id, to the elements which are requiring that the element be solved before they are.
     */
    private val solvesById = mutableMapOf<UUID, MutableSet<UUID>>()
    
    /**
     * Marks the given [elementId] as something which should exist in the ordering. The element must be registered
     * before it can be appended.
     */
    fun register(elementId: UUID)
    {
        dependenciesById[elementId] = mutableSetOf()
    }
    
    /**
     * Marks the given [elementId] as depending on [dependsOnId] being solved before it is. This method should be
     * invoked for every element in the source code which contains any dependencies. All dependencies should be recorded
     * in this manner.
     */
    fun append(elementId: UUID, dependsOnId: UUID)
    {
        dependenciesById[elementId]!! += dependsOnId
        solvesById.getOrPut(dependsOnId) { mutableSetOf() } += elementId
    }
    
    /**
     * Retrieves the set of dependencies which have no dependencies left. Any elements which were retrieved from this
     * method are removed from the ordering. A user should invoke this method until [Batch.Empty] is returned.
     */
    fun fetch(): Batch
    {
        if (dependenciesById.isEmpty())
            return Batch.Empty
        
        val candidates = dependenciesById.asSequence().filter { it.value.isEmpty() }.map { it.key }.toSet()
        if (candidates.isEmpty())
            return Batch.Invalid(dependenciesById)
        
        // Once we have found the set of candidates, we remove them from the dependency graph. This ensures that the
        // next fetch will be able to retrieve a fresh set of elements with no dependencies.
        for (candidate in candidates)
            solvesById[candidate]?.forEach { dependenciesById[it]?.let { set -> set -= it } }
        
        dependenciesById -= candidates
        solvesById -= candidates
        return Batch.Valid(candidates)
    }
}
