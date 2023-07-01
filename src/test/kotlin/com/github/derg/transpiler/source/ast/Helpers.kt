package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.phases.parser.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.thir.*

// Helper literals for generating an expression node from a primitive value
val Boolean.ast: AstExpression get() = AstBool(this)
val Int.ast: AstExpression get() = AstReal(this, Builtin.LIT_INT32)
val Long.ast: AstExpression get() = AstReal(this, Builtin.LIT_INT64)
val String.ast: AstExpression get() = AstText(this, null)

// Generates expressions from operations
fun astNot(that: Any) = AstNot(that.toExp())
fun astPlus(that: Any) = AstPlus(that.toExp())
fun astMinus(that: Any) = AstMinus(that.toExp())
infix fun Any.astEq(that: Any) = AstEqual(toExp(), that.toExp())
infix fun Any.astNe(that: Any) = AstNotEqual(toExp(), that.toExp())
infix fun Any.astLe(that: Any) = AstLessEqual(toExp(), that.toExp())
infix fun Any.astLt(that: Any) = AstLess(toExp(), that.toExp())
infix fun Any.astGe(that: Any) = AstGreaterEqual(toExp(), that.toExp())
infix fun Any.astGt(that: Any) = AstGreater(toExp(), that.toExp())
infix fun Any.astTw(that: Any) = AstThreeWay(toExp(), that.toExp())
infix fun Any.astAnd(that: Any) = AstAnd(toExp(), that.toExp())
infix fun Any.astOr(that: Any) = AstOr(toExp(), that.toExp())
infix fun Any.astXor(that: Any) = AstXor(toExp(), that.toExp())
infix fun Any.astAdd(that: Any) = AstAdd(toExp(), that.toExp())
infix fun Any.astSub(that: Any) = AstSubtract(toExp(), that.toExp())
infix fun Any.astMul(that: Any) = AstMultiply(toExp(), that.toExp())
infix fun Any.astDiv(that: Any) = AstDivide(toExp(), that.toExp())
infix fun Any.astMod(that: Any) = AstModulo(toExp(), that.toExp())
infix fun Any.astCatch(that: Any) = AstCatch(toExp(), that.toExp())
infix fun Any.astRaise(that: Any) = AstRaise(toExp(), that.toExp())

// Generates assignment from operations
infix fun Name.astAssign(that: Any) = AstAssign(this, that.toExp())
infix fun Name.astAssignAdd(that: Any) = AstAssign(this, AstAdd(AstRead(this), that.toExp()))
infix fun Name.astAssignSub(that: Any) = AstAssign(this, AstSubtract(AstRead(this), that.toExp()))
infix fun Name.astAssignMul(that: Any) = AstAssign(this, AstMultiply(AstRead(this), that.toExp()))
infix fun Name.astAssignMod(that: Any) = AstAssign(this, AstModulo(AstRead(this), that.toExp()))
infix fun Name.astAssignDiv(that: Any) = AstAssign(this, AstDivide(AstRead(this), that.toExp()))

// Generates statements from expressions
fun astInvokeOf(expression: Any) = AstEnter(expression.toExp())
fun astRaiseOf(expression: Any) = AstReturnError(expression.toExp())
fun astReturnOf(expression: Any? = null) = AstReturnValue(expression?.toExp())

/**
 * Generates segment definition from the provided input parameters.
 */
fun astSegmentOf(
    module: Name? = null,
    imports: List<Name> = emptyList(),
    statements: List<AstDefinition> = emptyList(),
) = AstSegment(
    module = module,
    imports = imports,
    definitions = statements,
)

/**
 * Generates type definition from the provided input parameters.
 */
fun astTypeOf(
    name: Name,
    vis: Visibility = Visibility.PRIVATE,
    props: List<AstProperty> = emptyList(),
) = AstType(
    name = name,
    visibility = vis,
    properties = props,
)

/**
 * Generates type property definition from the provided input parameters.
 */
fun astPropOf(
    name: Name,
    type: Name? = null,
    value: Any? = null,
    vis: Visibility = Visibility.PRIVATE,
    mut: Mutability = Mutability.IMMUTABLE,
    ass: Assignability = Assignability.CONSTANT,
) = AstProperty(
    name = name,
    type = type,
    value = value?.toExp(),
    visibility = vis,
    mutability = mut,
    assignability = ass,
)

/**
 * Generates variable definition from the provided input parameters.
 */
fun astVarOf(
    name: Name,
    value: Any,
    type: Name? = null,
    vis: Visibility = Visibility.PRIVATE,
    mut: Mutability = Mutability.IMMUTABLE,
    ass: Assignability = Assignability.CONSTANT,
) = AstVariable(
    name = name,
    type = type,
    value = value.toExp(),
    visibility = vis,
    mutability = mut,
    assignability = ass,
)

/**
 * Generates function definition from the provided input parameters.
 */
fun astFunOf(
    name: Name,
    valType: Name? = null,
    errType: Name? = null,
    vis: Visibility = Visibility.PRIVATE,
    params: List<AstParameter> = emptyList(),
    statements: List<AstStatement> = emptyList(),
) = AstFunction(
    name = name,
    valueType = valType,
    errorType = errType,
    parameters = params,
    visibility = vis,
    statements = statements,
)

/**
 * Generates function parameter definition from the provided input parameters.
 */
fun astParOf(
    name: Name,
    type: Name? = null,
    value: Any? = null,
    pas: Passability = Passability.IN,
    ass: Assignability = Assignability.CONSTANT,
) = AstParameter(
    name = name,
    type = type,
    value = value?.toExp(),
    passability = pas,
    assignability = ass,
)

/**
 * Generates a branch statement from the provided input parameters.
 */
fun astIfOf(predicate: Any, success: List<AstStatement>, failure: List<AstStatement> = emptyList()) =
    AstBranch(predicate.toExp(), success, failure)

/**
 * Generates a branch expression from the provided input parameters.
 */
fun astWhenOf(expression: Any, vararg branches: Pair<Any, Any>, default: Any? = null) =
    AstWhen(expression.toExp(), branches.map { it.first.toExp() to it.second.toExp() }, default?.toExp())
