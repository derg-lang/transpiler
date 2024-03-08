package com.github.derg.transpiler.phases.converter

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.hir.*
import java.util.*

/**
 * Converts the provided AST [segments] into a HIR package. The input segments are all used to form the single package.
 * Note that segments from another package, must be separately converted into the HIR structure.
 */
fun convert(segments: List<AstSegment>) = HirPackage(
    id = UUID.randomUUID(),
    name = "TODO - package name",
    modules = segments.groupBy { it.module ?: "TODO - module name" }.map { module(it.key, it.value) }
)

private fun module(name: String, segments: List<AstSegment>) = HirModule(
    id = UUID.randomUUID(),
    name = name,
    segments = segments.map { it.toHir() },
)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Implementation details
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

private fun AstType.toHir() = HirTypeStruct(
    name = name,
    generics = emptyList(),
    mutability = mutability,
)

/**
 *
 */
internal fun AstVariable.toHirConstant() = HirConstant(
    id = UUID.randomUUID(),
    name = name,
    type = type?.toHir(),
    value = value.toHir(),
    visibility = visibility,
)

/**
 *
 */
internal fun AstFunction.toHir() = HirFunction(
    id = UUID.randomUUID(),
    name = name,
    value = valueType?.toHir(),
    error = errorType?.toHir(),
    visibility = visibility,
    instructions = statements.map { it.toHir() },
    generics = emptyList(),
    variables = statements.filterIsInstance<AstVariable>().map { it.toHirVariable() },
    parameters = parameters.map { it.toHir() },
)

/**
 *
 */
internal fun AstParameter.toHir() = HirParameter(
    id = UUID.randomUUID(),
    name = name,
    type = type.toHir(),
    value = value?.toHir(),
    passability = passability,
)

/**
 *
 */
internal fun AstProperty.toHir() = HirField(
    id = UUID.randomUUID(),
    name = name,
    type = type.toHir(),
    value = value?.toHir(),
    visibility = visibility,
    assignability = assignability,
)

/**
 *
 */
internal fun AstSegment.toHir() = HirSegment(
    id = UUID.randomUUID(),
    name = "TODO - segment name",
    imports = imports.toSet(),
    structs = definitions.filterIsInstance<AstStruct>().map { it.toHir() },
    concepts = emptyList(),
    constants = definitions.filterIsInstance<AstVariable>().map { it.toHirConstant() },
    functions = definitions.filterIsInstance<AstFunction>().map { it.toHir() },
)

/**
 *
 */
internal fun AstStruct.toHir() = HirStruct(
    id = UUID.randomUUID(),
    name = name,
    visibility = visibility,
    fields = properties.map { it.toHir() },
    methods = emptyList(),
    generics = emptyList(),
)

/**
 *
 */
internal fun AstVariable.toHirVariable() = HirVariable(
    id = UUID.randomUUID(),
    name = name,
    type = type?.toHir(),
    value = value.toHir(),
    assignability = assignability,
)

/**
 * Converts [this] expression from AST to HIR. The data structure will be encoded with appropriate default information
 * where information is missing in the AST.
 */
internal fun AstValue.toHir(): HirValue = when (this)
{
    is AstCall         -> TODO()
    is AstRead         -> HirLoad(name, emptyList())
    is AstBool         -> HirBool(value)
    is AstInteger      -> HirInteger(value, literal)
    is AstDecimal      -> HirDecimal(value, literal)
    is AstText         -> HirText(value, literal)
    is AstAdd          -> HirAdd(lhs.toHir(), rhs.toHir())
    is AstAnd          -> HirAnd(lhs.toHir(), rhs.toHir())
    is AstCatch        -> TODO()
    is AstDivide       -> HirDiv(lhs.toHir(), rhs.toHir())
    is AstEqual        -> HirEq(lhs.toHir(), rhs.toHir())
    is AstGreater      -> HirGt(lhs.toHir(), rhs.toHir())
    is AstGreaterEqual -> HirGe(lhs.toHir(), rhs.toHir())
    is AstLess         -> HirLt(lhs.toHir(), rhs.toHir())
    is AstLessEqual    -> HirLe(lhs.toHir(), rhs.toHir())
    is AstMinus        -> HirMinus(expression.toHir())
    is AstModulo       -> HirMod(lhs.toHir(), rhs.toHir())
    is AstMultiply     -> HirMul(lhs.toHir(), rhs.toHir())
    is AstNot          -> HirNot(expression.toHir())
    is AstNotEqual     -> HirNe(lhs.toHir(), rhs.toHir())
    is AstOr           -> HirOr(lhs.toHir(), rhs.toHir())
    is AstPlus         -> HirPlus(expression.toHir())
    is AstRaise        -> TODO()
    is AstSubtract     -> HirSub(lhs.toHir(), rhs.toHir())
    is AstThreeWay     -> TODO()
    is AstXor          -> HirXor(lhs.toHir(), rhs.toHir())
    is AstWhen         -> TODO()
}

/**
 * Converts [this] statement from AST to HIR. The data structure will be encoded with appropriate default information
 * where information is missing in the AST.
 */
internal fun AstInstruction.toHir(): HirInstruction = when (this)
{
    is AstAssign      -> HirAssign(HirLoad(name, emptyList()), expression.toHir())
    is AstBranch      -> HirBranch(predicate.toHir(), success.map { it.toHir() }, failure.map { it.toHir() })
    is AstEvaluate    -> HirEvaluate(expression.toHir())
    is AstReturn      -> HirReturn
    is AstReturnError -> HirReturnError(expression.toHir())
    is AstReturnValue -> HirReturnValue(expression.toHir())
    is AstVariable    -> HirAssign(HirLoad(name, emptyList()), value.toHir())
}
