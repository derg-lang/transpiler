package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.utils.*
import java.math.*
import java.util.*

/////////////////////
// Literal helpers //
/////////////////////

val Any.hir: HirExpression
    get() = when (this)
    {
        is HirExpression -> this
        is Boolean       -> HirExpression.Bool(UUID.randomUUID(), this)
        is Int           -> hirIntegerOf(value = toBigInteger(), literal = INT32_LIT_NAME)
        is Long          -> hirIntegerOf(value = toBigInteger(), literal = INT64_LIT_NAME)
        is Float         -> hirDecimalOf(value = toBigDecimal(), literal = FLOAT32_LIT_NAME)
        is Double        -> hirDecimalOf(value = toBigDecimal(), literal = FLOAT64_LIT_NAME)
        is String        -> HirExpression.Text(UUID.randomUUID(), this, STR_LIT_NAME)
        else             -> throw IllegalArgumentException("Value $this does not represent a valid hir value")
    }

fun hirDecimalOf(
    id: UUID = UUID.randomUUID(),
    value: BigDecimal = BigDecimal.ZERO,
    literal: String = INT64_LIT_NAME,
) = HirExpression.Decimal(
    id = id,
    value = value,
    literal = literal,
)

fun hirIntegerOf(
    id: UUID = UUID.randomUUID(),
    value: BigInteger = BigInteger.ZERO,
    literal: String = INT64_LIT_NAME,
) = HirExpression.Integer(
    id = id,
    value = value,
    literal = literal,
)

//////////////////
// Type helpers //
//////////////////

fun HirExpression.hirType() = HirType.Expression(this)

////////////////////////
// Expression helpers //
////////////////////////

val Any.hirNot: HirExpression get() = hirUnary(this.hir, UnaryOperator.NOT)
val Any.hirPlus: HirExpression get() = hirUnary(this.hir, UnaryOperator.PLUS)
val Any.hirMinus: HirExpression get() = hirUnary(this.hir, UnaryOperator.MINUS)

infix fun Any.hirEq(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.EQUAL)
infix fun Any.hirNe(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.NOT_EQUAL)
infix fun Any.hirGe(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.GREATER)
infix fun Any.hirGt(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.GREATER_EQUAL)
infix fun Any.hirLe(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.LESS)
infix fun Any.hirLt(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.LESS_EQUAL)

infix fun Any.hirAdd(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.ADD)
infix fun Any.hirDiv(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.DIVIDE)
infix fun Any.hirMod(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.MODULO)
infix fun Any.hirMul(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.MULTIPLY)
infix fun Any.hirSub(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.SUBTRACT)

infix fun Any.hirAnd(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.AND)
infix fun Any.hirOr(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.OR)
infix fun Any.hirXor(that: Any) = hirBinary(this.hir, that.hir, BinaryOperator.XOR)

infix fun Any.hirCatch(that: Any) = HirExpression.Catch(UUID.randomUUID(), this.hir, that.hir, CatchOperator.HANDLE)
infix fun Any.hirCatchError(that: Any) = HirExpression.Catch(UUID.randomUUID(), this.hir, that.hir, CatchOperator.RETURN_ERROR)
infix fun Any.hirCatchValue(that: Any) = HirExpression.Catch(UUID.randomUUID(), this.hir, that.hir, CatchOperator.RETURN_VALUE)

fun String.hirIdent(vararg parameters: NamedMaybe<Any>) = HirExpression.Identifier(UUID.randomUUID(), this, parameters.map { it.first to it.second.hir })
fun HirExpression.hirCall(vararg parameters: NamedMaybe<Any>) = HirExpression.Call(UUID.randomUUID(), this, parameters.map { it.first to it.second.hir })
fun HirExpression.hirMember(field: HirExpression.Identifier) = HirExpression.Field(UUID.randomUUID(), this, field)

private fun hirBinary(lhs: HirExpression, rhs: HirExpression, operator: BinaryOperator): HirExpression
{
    val instance = HirExpression.Identifier(UUID.randomUUID(), operator.symbol, emptyList())
    return HirExpression.Call(UUID.randomUUID(), instance, listOf(null to lhs, null to rhs))
}

private fun hirUnary(rhs: HirExpression, operator: UnaryOperator): HirExpression
{
    val instance = HirExpression.Identifier(UUID.randomUUID(), operator.symbol, emptyList())
    return HirExpression.Call(UUID.randomUUID(), instance, listOf(null to rhs))
}

///////////////////////
// Statement helpers //
///////////////////////

private fun hirAssign(lhs: HirExpression, rhs: HirExpression) = HirStatement.Assign(lhs, rhs, AssignOperator.EQUAL)

val Any.hirEval get() = HirStatement.Evaluate(hir)
val Any.hirReturnError get() = HirStatement.ReturnError(hir)
val Any.hirReturnValue get() = HirStatement.ReturnValue(hir)

fun Any.hirIf(
    success: List<HirStatement> = emptyList(),
    failure: List<HirStatement> = emptyList(),
) = HirStatement.If(hir, success, failure)

fun Any.hirWhile(
    statements: List<HirStatement> = emptyList(),
) = HirStatement.While(hir, statements)

/////////////////////////
// Declaration helpers //
/////////////////////////

fun hirConstOf(
    name: String = UUID.randomUUID().toString(),
    type: HirType? = INT32_TYPE_NAME.hirIdent().hirType(),
    value: HirExpression = 0.hir,
) = HirDeclaration.ConstantDecl(
    id = UUID.randomUUID(),
    name = name,
    type = type,
    value = value,
)

fun hirFieldOf(
    name: String = UUID.randomUUID().toString(),
    type: HirType = INT32_TYPE_NAME.hirIdent().hirType(),
    default: HirExpression? = null,
) = HirDeclaration.FieldDecl(
    id = UUID.randomUUID(),
    name = name,
    type = type,
    default = default,
    visibility = Visibility.PRIVATE,
    assignability = Assignability.FINAL,
)

fun hirFunOf(
    name: String = UUID.randomUUID().toString(),
    typeParameters: List<HirDeclaration.TypeParameterDecl> = emptyList(),
    parameters: List<HirDeclaration.ParameterDecl> = emptyList(),
    valueType: HirType? = null,
    errorType: HirType? = null,
    statements: List<HirStatement> = emptyList(),
) = HirDeclaration.FunctionDecl(
    id = UUID.randomUUID(),
    name = name,
    visibility = Visibility.PRIVATE,
    typeParameters = typeParameters,
    parameters = parameters,
    valueType = valueType,
    errorType = errorType,
    body = statements,
)

fun hirLitOf(
    name: String = UUID.randomUUID().toString(),
    parameter: HirDeclaration.ParameterDecl = hirParamOf(),
    valueType: HirType = INT32_TYPE_NAME.hirIdent().hirType(),
    body: List<HirStatement> = emptyList(),
) = HirDeclaration.FunctionDecl(
    id = UUID.randomUUID(),
    name = name,
    visibility = Visibility.PRIVATE,
    typeParameters = emptyList(),
    parameters = listOf(parameter),
    valueType = valueType,
    errorType = null,
    body = body,
)

fun hirParamOf(
    name: String = UUID.randomUUID().toString(),
    type: HirType = INT32_TYPE_NAME.hirIdent().hirType(),
    default: HirExpression? = null,
) = HirDeclaration.ParameterDecl(
    id = UUID.randomUUID(),
    name = name,
    type = type,
    default = default,
    passability = Passability.IN,
)

fun hirSegmentOf(
    imports: List<String> = emptyList(),
    constants: List<HirDeclaration.ConstantDecl> = emptyList(),
    functions: List<HirDeclaration.FunctionDecl> = emptyList(),
    structures: List<HirDeclaration.StructureDecl> = emptyList(),
) = HirDeclaration.SegmentDecl(
    id = UUID.randomUUID(),
    imports = imports,
    constants = constants,
    functions = functions,
    structures = structures,
)

fun hirStructOf(
    name: String = UUID.randomUUID().toString(),
    typeParameters: List<HirDeclaration.TypeParameterDecl> = emptyList(),
    fields: List<HirDeclaration.FieldDecl> = emptyList(),
) = HirDeclaration.StructureDecl(
    id = UUID.randomUUID(),
    name = name,
    typeParameters = typeParameters,
    fields = fields,
    visibility = Visibility.PRIVATE,
)

fun hirVarOf(
    name: String = UUID.randomUUID().toString(),
    type: HirType? = null,
    value: HirExpression = 0.hir,
) = HirStatement.Variable(
    id = UUID.randomUUID(),
    name = name,
    type = type,
    value = value,
    assignability = Assignability.FINAL,
)
