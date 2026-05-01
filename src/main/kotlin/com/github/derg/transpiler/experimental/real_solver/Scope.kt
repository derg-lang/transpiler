package com.github.derg.transpiler.experimental.real_solver

import com.github.derg.transpiler.source.hir.*
import java.util.*

/**
 * Generates a map from all symbols, to the scope the symbols are visible within. These scopes are used for visibility
 * purposes, indicating which symbols are visible from where.
 *
 * @param scope The package scope, holding all entries within the package. This scope will not be modified.
 * @param module The module which should be converted into a scope tree of visibilities.
 */
internal fun scope(scope: Scope, module: HirDeclaration.ModuleDecl): Scoping
{
    val scopes = mutableMapOf<UUID, Scope>()
    
    fun store(outer: Scope, expression: HirExpression)
    {
        scopes[expression.id] = outer
    }
    
    fun store(outer: Scope, statement: HirStatement): Unit = when (statement)
    {
        is HirStatement.Return      -> Unit
        is HirStatement.ReturnError -> store(outer, statement.expression)
        is HirStatement.ReturnValue -> store(outer, statement.expression)
        is HirStatement.Evaluate    -> store(outer, statement.expression)
        is HirStatement.Variable    -> store(outer, statement.value)
        is HirStatement.Assign      ->
        {
            store(outer, statement.instance)
            store(outer, statement.expression)
        }
        is HirStatement.For         ->
        {
            val inner = Scope(outer)
            store(outer, statement.expression)
            statement.body.forEach { store(inner, it) }
        }
        is HirStatement.If          ->
        {
            val innerThen = Scope(outer)
            val innerElse = Scope(outer)
            store(outer, statement.predicate)
            statement.success.forEach { store(innerThen, it) }
            statement.failure.forEach { store(innerElse, it) }
        }
        is HirStatement.While       ->
        {
            val inner = Scope(outer)
            store(outer, statement.predicate)
            statement.body.forEach { store(inner, it) }
        }
    }.also { scopes[statement.id] = outer }
    
    fun store(outer: Scope, symbol: HirDeclaration): Unit = when (symbol)
    {
        is HirDeclaration.ConstantDecl      -> store(outer, symbol.value)
        is HirDeclaration.FieldDecl         -> symbol.default?.let { store(outer, it) } ?: Unit
        is HirDeclaration.ParameterDecl     -> symbol.default?.let { store(outer, it) } ?: Unit
        is HirDeclaration.TypeParameterDecl -> symbol.default?.let { store(outer, it) } ?: Unit
        is HirDeclaration.ModuleDecl        ->
        {
            val inner = Scope(outer)
            for (segment in symbol.segments)
            {
                segment.constants.forEach { store(inner, it) }
                segment.functions.forEach { store(inner, it) }
                segment.structures.forEach { store(inner, it) }
            }
        }
        is HirDeclaration.StructureDecl     ->
        {
            val inner = Scope(outer)
            
            symbol.ctorEntries.forEach { store(inner, it) }
            symbol.fields.forEach { store(inner, it) }
            symbol.methods.forEach { store(inner, it) }
            symbol.typeParameters.forEach { store(inner, it) }
        }
        is HirDeclaration.FunctionDecl      ->
        {
            val inner = Scope(outer)
            symbol.parameters.forEach { store(inner, it) }
            symbol.typeParameters.forEach { store(inner, it) }
            symbol.body.forEach { store(inner, it) }
        }
        is HirDeclaration.MethodDecl        ->
        {
            val inner = Scope(outer)
            symbol.parameters.forEach { store(inner, it) }
            symbol.typeParameters.forEach { store(inner, it) }
            symbol.body.forEach { store(inner, it) }
        }
    }.also { scopes[symbol.id] = outer }
    
    store(scope, module)
    return scopes
}

/**
 * A mapping from an element to the scope the element is present in. This mapping is useful to go from an arbitrary
 * element in source code, to the visibility applicable to said element.
 */
typealias Scoping = Map<UUID, Scope>

/**
 * Scopes represents the symbols which are accessible at a certain layer in the source code. Scopes may be nested inside
 * each other, forming a hierarchy of scopes.
 *
 * @param parent The parent to this scope; all elements in the parent will be visible within this scope.
 */
class Scope(private val parent: Scope?)
{
    private val nameToSymbols = mutableMapOf<String, MutableSet<UUID>>()
    
    /**
     * Registers a [name] in this scope, bound to the given [id]. If multiple symbols are registered under the same
     * name, they will all be overloaded in this scope. That is, the same name may refer to multiple symbols at the
     * same time.
     */
    fun register(id: UUID, name: String)
    {
        nameToSymbols.getOrPut(name) { mutableSetOf() }.add(id)
    }
    
    /**
     * Retrieves all symbols which are registered under the given name.
     */
    fun find(name: String): Set<UUID>
    {
        return nameToSymbols[name].orEmpty() + parent?.find(name).orEmpty()
    }
}
