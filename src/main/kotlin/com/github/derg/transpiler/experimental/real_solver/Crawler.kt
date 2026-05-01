package com.github.derg.transpiler.experimental.real_solver

import com.github.derg.transpiler.source.hir.*
import java.util.*

/**
 *
 */
class Crawler(private val pkg: Scope)
{
    private val scoping = mutableMapOf<UUID, Scope>()
    private val ordering = Ordering()
    
    /**
     * Retrieves the scope containing the given [elementId]. All in a given module are assumed to be incorporated into
     * the collector.
     */
    fun getScope(elementId: UUID): Scope =
        scoping[elementId] ?: throw IllegalArgumentException("Cannot find scope for element '$elementId'")
    
    /**
     * Runs over the entire [module], collecting all information required for type-checking. The collection routine will
     * only make the information available internally, the user is required to access information on a need-to-know
     * basis.
     */
    fun crawl(module: HirDeclaration.ModuleDecl)
    {
        visit(pkg, module)
    }
    
    private fun visit(outer: Scope, symbol: HirDeclaration)
    {
        outer.register(symbol.id, symbol.name)
        scoping[symbol.id] = outer
        ordering.register(symbol.id)
        
        when (symbol)
        {
            is HirDeclaration.ConstantDecl      ->
            {
                symbol.kind?.let { visit(outer, it, symbol) }
                symbol.value.also { visit(outer, it) }
            }
            is HirDeclaration.FieldDecl         ->
            {
                symbol.kind?.let { visit(outer, it, symbol) }
                symbol.default?.let { visit(outer, it) }
            }
            is HirDeclaration.ParameterDecl     ->
            {
                symbol.kind?.let { visit(outer, it, symbol) }
                symbol.default?.let { visit(outer, it) }
            }
            is HirDeclaration.TypeParameterDecl ->
            {
                symbol.kind.also { visit(outer, it, symbol) }
                symbol.default?.let { visit(outer, it) }
            }
            is HirDeclaration.ModuleDecl        ->
            {
                val inner = Scope(outer)
                
                for (segment in symbol.segments)
                {
                    segment.constants.forEach { visit(inner, it) }
                    segment.functions.forEach { visit(inner, it) }
                    segment.structures.forEach { visit(inner, it) }
                }
            }
            is HirDeclaration.StructureDecl     ->
            {
                val inner = Scope(outer)
                
                symbol.ctorEntries.forEach { visit(inner, it) }
                symbol.fields.forEach { visit(inner, it) }
                symbol.methods.forEach { visit(inner, it) }
                symbol.typeParameters.forEach { visit(inner, it) }
            }
            is HirDeclaration.FunctionDecl      ->
            {
                val inner = Scope(outer)
                
                symbol.typeParameters.forEach { visit(inner, it) }
                symbol.parameters.forEach { visit(inner, it) }
                symbol.body.forEach { visit(inner, it, symbol) }
                symbol.valueKind?.let { visit(outer, it, symbol) }
                symbol.errorKind?.let { visit(outer, it, symbol) }
            }
            is HirDeclaration.MethodDecl        ->
            {
                val inner = Scope(outer)
                
                symbol.typeParameters.forEach { visit(inner, it) }
                symbol.parameters.forEach { visit(inner, it) }
                symbol.body.forEach { visit(inner, it, symbol) }
                symbol.valueKind?.let { visit(outer, it, symbol) }
                symbol.errorKind?.let { visit(outer, it, symbol) }
            }
        }
    }
    
    private fun visit(outer: Scope, statement: HirStatement, symbol: HirDeclaration)
    {
        ordering.register(statement.id)
    }
    
    private fun visit(outer: Scope, expression: HirExpression)
    {
        ordering.register(expression.id)
    }
    
    private fun visit(outer: Scope, kind: HirKind, symbol: HirDeclaration)
    {
        if (kind !is HirKind.Value)
            return
        
        when (kind.type)
        {
            is HirType.Expression -> TODO()
            is HirType.Function   ->
            {
                kind.type.valueKind?.let { visit(outer, it, symbol) }
                kind.type.errorKind?.let { visit(outer, it, symbol) }
                kind.type.parameters.forEach { visit(outer, it, symbol) }
            }
        }
    }
}
