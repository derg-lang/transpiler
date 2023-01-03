package com.github.derg.transpiler.source.lir

import com.github.derg.transpiler.source.*

data class Module(
    val id: IdModule,
) : Symbol

data class Variable(
    val id: IdVariable,
    val type: IdType,
    val visibility: Visibility,
    val mutability: Mutability,
) : Symbol

data class Function(
    val id: IdFunction,
    val valueType: IdType,
    val errorType: IdType,
    val parameters: List<Parameter>,
    val visibility: Visibility,
) : Symbol

data class Parameter(
    val id: IdParameter,
    val type: IdType,
    val mutability: Mutability,
) : Symbol
