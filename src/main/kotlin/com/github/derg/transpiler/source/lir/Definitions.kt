package com.github.derg.transpiler.source.lir

import com.github.derg.transpiler.core.Id
import com.github.derg.transpiler.core.Name
import com.github.derg.transpiler.source.Mutability
import com.github.derg.transpiler.source.Visibility

data class Module(
    val name: Name,
    val id: Id,
) : Symbol

data class Variable(
    val name: Name,
    val id: Id,
    val type: Id,
    val visibility: Visibility,
    val mutability: Mutability,
) : Symbol

data class Function(
    val name: Name,
    val id: Id,
    val valueType: Id,
    val errorType: Id,
    val parameters: List<Parameter>,
    val visibility: Visibility,
) : Symbol

data class Parameter(
    val name: Name,
    val id: Id,
    val type: Id,
    val mutability: Mutability,
) : Symbol
