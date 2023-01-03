package com.github.derg.transpiler.source.lir

import com.github.derg.transpiler.source.*

/**
 * All named objects within a codebase is known as a symbol. The symbols are given an identifier, which allows them to
 * be retrieved at a later time.
 */
sealed interface Symbol
{
    /**
     * The unique id representing the symbol.
     */
    val id: Id
}

/**
 *
 */
data class Module(
    override val id: IdModule,
) : Symbol

/**
 *
 */
data class Variable(
    override val id: IdVariable,
    val type: IdType,
    val visibility: Visibility,
    val mutability: Mutability,
) : Symbol

/**
 *
 */
data class Function(
    override val id: IdFunction,
    val valueType: IdType,
    val errorType: IdType,
    val parameters: List<Parameter>,
    val visibility: Visibility,
) : Symbol

/**
 *
 */
data class Parameter(
    override val id: IdParameter,
    val type: IdType,
    val mutability: Mutability,
) : Symbol
