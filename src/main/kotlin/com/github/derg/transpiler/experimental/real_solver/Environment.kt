package com.github.derg.transpiler.experimental.real_solver

import com.github.derg.transpiler.source.thir.*
import java.util.*

/**
 * The environment stores and holds all information required during type-checking.
 */
class Environment
{
    val constants = mutableMapOf<UUID, ThirDeclaration.Const>()
    val functions = mutableMapOf<UUID, ThirDeclaration.Function>()
    val structures = mutableMapOf<UUID, ThirDeclaration.Structure>()
}
