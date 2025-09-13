package com.github.derg.transpiler.phases.converter

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.hir.*
import java.util.*

/**
 * Converts the provided AST [program] into a HIR program. The input segments are all used to form the single package.
 * Note that segments from another package, must be separately converted into the HIR structure.
 */
fun convert(program: AstSegment): HirDeclaration.SegmentDecl
{
    return program.toHir()
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Implementation details
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

private fun AstConstant.toHir() = HirDeclaration.ConstantDecl(
    id = UUID.randomUUID(),
    name = name,
    type = type?.toHir(),
    value = value.toHir(),
)

private fun AstFunction.toHir() = HirDeclaration.FunctionDecl(
    id = UUID.randomUUID(),
    name = name,
    visibility = visibility,
    typeParameters = emptyList(),
    parameters = parameters.map { it.toHir() },
    valueType = valueType?.toHir(),
    errorType = errorType?.toHir(),
    body = statements.map { it.toHir() },
)

private fun AstParameter.toHir() = HirDeclaration.ParameterDecl(
    id = UUID.randomUUID(),
    name = name,
    type = type.toHir(),
    default = value?.toHir(),
    passability = passability,
)

private fun AstProperty.toHir() = HirDeclaration.FieldDecl(
    id = UUID.randomUUID(),
    name = name,
    type = type.toHir(),
    default = value?.toHir(),
    visibility = visibility,
    assignability = assignability,
)

private fun AstSegment.toHir() = HirDeclaration.SegmentDecl(
    id = UUID.randomUUID(),
    imports = imports,
    constants = definitions.filterIsInstance<AstConstant>().map { it.toHir() },
    functions = definitions.filterIsInstance<AstFunction>().map { it.toHir() },
    structures = definitions.filterIsInstance<AstStruct>().map { it.toHir() },
)

private fun AstStruct.toHir() = HirDeclaration.StructureDecl(
    id = UUID.randomUUID(),
    name = name,
    typeParameters = templates.map { it.toHir() },
    fields = fields.map { it.toHir() },
    visibility = visibility,
)

private fun AstVariable.toHir() = HirStatement.Variable(
    id = UUID.randomUUID(),
    name = name,
    type = type?.toHir(),
    value = value.toHir(),
    assignability = assignability,
)

private fun AstType.toHir(): HirType = when (this)
{
    is AstType.Expression -> HirType.Expression(value.toHir())
    is AstType.Function   -> HirType.Function(value?.toHir(), error?.toHir(), parameters.map { it.toHir() })
    is AstType.Type       -> HirType.Type
    is AstType.Union      -> TODO()
}

private fun AstTemplate.toHir(): HirDeclaration.TypeParameterDecl = when (this)
{
    is AstTemplate.Type  -> HirDeclaration.TypeParameterDecl(UUID.randomUUID(), name, null, null)
    is AstTemplate.Value -> HirDeclaration.TypeParameterDecl(UUID.randomUUID(), name, type.toHir(), default?.toHir())
}

private fun AstParameterDynamic.toHir(): HirType.Parameter =
    HirType.Parameter(name, type.toHir(), null, passability)

/**
 * Converts [this] expression from AST to HIR. The data structure will be encoded with appropriate default information
 * where information is missing in the AST.
 */
internal fun AstValue.toHir(): HirExpression = when (this)
{
    is AstCall         -> HirExpression.Call(UUID.randomUUID(), instance.toHir(), parameters.map { (name, value) -> name to value.toHir() })
    is AstLoad         -> toHirLoad()
    is AstMember       -> HirExpression.Field(UUID.randomUUID(), instance.toHir(), field.toHirLoad())
    is AstCatch        -> HirExpression.Catch(UUID.randomUUID(), lhs.toHir(), rhs.toHir(), operator)
    is AstBool         -> HirExpression.Bool(UUID.randomUUID(), value)
    is AstInteger      -> HirExpression.Integer(UUID.randomUUID(), value, literal)
    is AstDecimal      -> HirExpression.Decimal(UUID.randomUUID(), value, literal)
    is AstText         -> HirExpression.Text(UUID.randomUUID(), value, literal)
    is AstAdd          -> toHirCall(BinaryOperator.ADD, lhs, rhs)
    is AstAnd          -> toHirCall(BinaryOperator.AND, lhs, rhs)
    is AstDivide       -> toHirCall(BinaryOperator.DIVIDE, lhs, rhs)
    is AstEqual        -> toHirCall(BinaryOperator.EQUAL, lhs, rhs)
    is AstGreater      -> toHirCall(BinaryOperator.GREATER, lhs, rhs)
    is AstGreaterEqual -> toHirCall(BinaryOperator.GREATER_EQUAL, lhs, rhs)
    is AstLess         -> toHirCall(BinaryOperator.LESS, lhs, rhs)
    is AstLessEqual    -> toHirCall(BinaryOperator.LESS_EQUAL, lhs, rhs)
    is AstMinus        -> toHirCall(UnaryOperator.MINUS, expression)
    is AstModulo       -> toHirCall(BinaryOperator.MODULO, lhs, rhs)
    is AstMultiply     -> toHirCall(BinaryOperator.MULTIPLY, lhs, rhs)
    is AstNot          -> toHirCall(UnaryOperator.NOT, expression)
    is AstNotEqual     -> toHirCall(BinaryOperator.NOT_EQUAL, lhs, rhs)
    is AstOr           -> toHirCall(BinaryOperator.OR, lhs, rhs)
    is AstPlus         -> toHirCall(UnaryOperator.PLUS, expression)
    is AstSubtract     -> toHirCall(BinaryOperator.SUBTRACT, lhs, rhs)
    is AstThreeWay     -> TODO()
    is AstXor          -> toHirCall(BinaryOperator.XOR, lhs, rhs)
    is AstWhen         -> TODO()
}

private fun AstLoad.toHirLoad() =
    HirExpression.Identifier(UUID.randomUUID(), name, parameters.map { (name, value) -> name to value.toHir() })

// TODO: Replace all AST binary operators with `AstBinary`
private fun AstValue.toHirCall(operator: BinaryOperator, lhs: AstValue, rhs: AstValue): HirExpression
{
    val instance = HirExpression.Identifier(UUID.randomUUID(), operator.symbol, emptyList())
    return HirExpression.Call(UUID.randomUUID(), instance, listOf(null to lhs.toHir(), null to rhs.toHir()))
}

// TODO: Replace all AST unary operators with `AstUnary`
private fun AstValue.toHirCall(operator: UnaryOperator, rhs: AstValue): HirExpression
{
    val instance = HirExpression.Identifier(UUID.randomUUID(), operator.symbol, emptyList())
    return HirExpression.Call(UUID.randomUUID(), instance, listOf(null to rhs.toHir()))
}

/**
 * Converts [this] statement from AST to HIR. The data structure will be encoded with appropriate default information
 * where information is missing in the AST.
 */
internal fun AstInstruction.toHir(): HirStatement = when (this)
{
    is AstAssign         -> HirStatement.Assign(name.toHirLoad(), expression.toHir(), AssignOperator.EQUAL)
    is AstAssignDivide   -> HirStatement.Assign(name.toHirLoad(), expression.toHir(), AssignOperator.DIVIDE)
    is AstAssignSubtract -> HirStatement.Assign(name.toHirLoad(), expression.toHir(), AssignOperator.SUBTRACT)
    is AstAssignModulo   -> HirStatement.Assign(name.toHirLoad(), expression.toHir(), AssignOperator.MODULO)
    is AstAssignMultiply -> HirStatement.Assign(name.toHirLoad(), expression.toHir(), AssignOperator.MULTIPLY)
    is AstAssignAdd      -> HirStatement.Assign(name.toHirLoad(), expression.toHir(), AssignOperator.ADD)
    is AstBranch         -> HirStatement.If(predicate.toHir(), success.toHir(), failure.toHir())
    is AstEvaluate       -> HirStatement.Evaluate(expression.toHir())
    is AstFor            -> HirStatement.For(identifier, expression.toHir(), instructions.toHir())
    is AstReturn         -> HirStatement.Return
    is AstReturnError    -> HirStatement.ReturnError(expression.toHir())
    is AstReturnValue    -> HirStatement.ReturnValue(expression.toHir())
    is AstVariable       -> toHir()
    is AstWhile          -> HirStatement.While(predicate.toHir(), instructions.toHir())
}

private fun String.toHirLoad() =
    HirExpression.Identifier(UUID.randomUUID(), this, emptyList())

private fun List<AstInstruction>.toHir() =
    map { it.toHir() }
