package com.github.derg.transpiler.phases.converter

import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.hir.*
import java.util.*

/**
 * Converts the provided AST [program] into a HIR program. The input segments are all used to form the single package.
 * Note that segments from another package, must be separately converted into the HIR structure.
 */
fun convert(program: AstProgram) = HirProgram(
    applications = program.applications.map { it.toHir() },
    packages = program.packages.map { it.toHir() },
)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Implementation details
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 *
 */
private fun AstType.toHir(): HirType = when (this)
{
    is AstType.Function  -> HirType.Function(value = value?.toHir(), error = error?.toHir(), parameters = parameters.map { it.toHir() })
    is AstType.Structure -> HirType.Structure(name = name, mutability = mutability, parameters = parameters.map { it.toHir() })
    is AstType.Union     -> HirType.Union(types = types.map { it.toHir() }.toSet())
}

private fun AstParameterStatic.toHir() = HirParameterStatic(name = name, value = value.toHir())
private fun AstParameterDynamic.toHir() = HirParameterDynamic(name = name, type = type.toHir(), passability = passability)

/**
 *
 */
internal fun AstConstant.toHir() = HirConstant(
    id = UUID.randomUUID(),
    name = name,
    type = type.toHir(),
    value = value.toHir(),
    visibility = visibility,
)

/**
 *
 */
internal fun AstFunction.toHir() = HirFunction(
    id = UUID.randomUUID(),
    name = name,
    type = HirType.Function(
        value = valueType?.toHir(),
        error = errorType?.toHir(),
        parameters = parameters.map { HirParameterDynamic(it.name, it.type.toHir(), it.passability) },
    ),
    visibility = visibility,
    instructions = statements.map { it.toHir() },
    generics = emptyList(),
    variables = statements.filterIsInstance<AstVariable>().map { it.toHir() },
    parameters = parameters.map { it.toHir() },
)

/**
 *
 */
internal fun AstModule.toHir() = HirModule(
    id = UUID.randomUUID(),
    name = name,
    segments = segments.map { it.toHir() },
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
internal fun AstPackage.toHir() = HirPackage(
    id = UUID.randomUUID(),
    name = name,
    modules = modules.map { it.toHir() },
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
    imports = imports.toSet(),
    structs = definitions.filterIsInstance<AstStruct>().map { it.toHir() },
    concepts = emptyList(),
    constants = definitions.filterIsInstance<AstConstant>().map { it.toHir() },
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
internal fun AstVariable.toHir() = HirVariable(
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
    is AstCall         -> HirCall(instance.toHir(), parameters.map { it.name to it.expression.toHir() })
    is AstLoad         -> HirLoad(name, parameters.map { it.name to it.expression.toHir() })
    is AstBool         -> HirBool(value)
    is AstInteger      -> HirInteger(value, literal)
    is AstDecimal      -> HirDecimal(value, literal)
    is AstText         -> HirText(value, literal)
    is AstAdd          -> HirAdd(lhs.toHir(), rhs.toHir())
    is AstAnd          -> HirAnd(lhs.toHir(), rhs.toHir())
    is AstCatch        -> HirCatch(lhs.toHir(), rhs.toHir(), capture)
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
    is AstAssign         -> HirAssign(HirLoad(name, emptyList()), expression.toHir())
    is AstAssignDivide   -> HirAssignDivide(HirLoad(name, emptyList()), expression.toHir())
    is AstAssignSubtract -> HirAssignSubtract(HirLoad(name, emptyList()), expression.toHir())
    is AstAssignModulo   -> HirAssignModulo(HirLoad(name, emptyList()), expression.toHir())
    is AstAssignMultiply -> HirAssignMultiply(HirLoad(name, emptyList()), expression.toHir())
    is AstAssignAdd      -> HirAssignAdd(HirLoad(name, emptyList()), expression.toHir())
    is AstBranch         -> HirBranch(predicate.toHir(), success.map { it.toHir() }, failure.map { it.toHir() })
    is AstEvaluate       -> HirEvaluate(expression.toHir())
    is AstReturn         -> HirReturn
    is AstReturnError    -> HirReturnError(expression.toHir())
    is AstReturnValue    -> HirReturnValue(expression.toHir())
    is AstVariable       -> HirAssign(HirLoad(name, emptyList()), value.toHir())
}
