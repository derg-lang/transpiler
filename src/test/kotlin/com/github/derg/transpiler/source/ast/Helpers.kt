package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.thir.*
import java.util.*

/////////////////////
// Literal helpers //
/////////////////////

val Any.ast: AstExpression
    get() = when (this)
    {
        is AstExpression -> this
        is Boolean       -> AstBool(this)
        is Int           -> AstInteger(toBigInteger(), Builtin.INT32_LIT.name)
        is Long          -> AstInteger(toBigInteger(), Builtin.INT64_LIT.name)
        is String        -> AstText(this, Builtin.STR_LIT.name)
        else             -> throw IllegalArgumentException("Value $this does not represent a valid ast value")
    }

////////////////////////
// Expression helpers //
////////////////////////

val Any.astNot get() = AstNot(ast)
val Any.astPlus get() = AstPlus(ast)
val Any.astMinus get() = AstMinus(ast)

infix fun Any.astEq(that: Any) = AstEqual(this.ast, that.ast)
infix fun Any.astNe(that: Any) = AstNotEqual(this.ast, that.ast)
infix fun Any.astLe(that: Any) = AstLessEqual(this.ast, that.ast)
infix fun Any.astLt(that: Any) = AstLess(this.ast, that.ast)
infix fun Any.astGe(that: Any) = AstGreaterEqual(this.ast, that.ast)
infix fun Any.astGt(that: Any) = AstGreater(this.ast, that.ast)
infix fun Any.astTw(that: Any) = AstThreeWay(this.ast, that.ast)
infix fun Any.astAnd(that: Any) = AstAnd(this.ast, that.ast)
infix fun Any.astOr(that: Any) = AstOr(this.ast, that.ast)
infix fun Any.astXor(that: Any) = AstXor(this.ast, that.ast)
infix fun Any.astAdd(that: Any) = AstAdd(this.ast, that.ast)
infix fun Any.astSub(that: Any) = AstSubtract(this.ast, that.ast)
infix fun Any.astMul(that: Any) = AstMultiply(this.ast, that.ast)
infix fun Any.astDiv(that: Any) = AstDivide(this.ast, that.ast)
infix fun Any.astMod(that: Any) = AstModulo(this.ast, that.ast)
infix fun Any.astCatch(that: Any) = AstCatch(this.ast, that.ast)
infix fun Any.astRaise(that: Any) = AstRaise(this.ast, that.ast)

val String.astRead: AstRead get() = AstRead(this)
fun String.astCall(vararg valArgs: Any) = AstCall(this, emptyList(), valArgs.map(Any::astArg))

val Any.astArg: AstArgument
    get() = if (this is Pair<*, *>) AstArgument(first as String, (second as Any).ast) else AstArgument(null, ast)

///////////////////////
// Statement helpers //
///////////////////////

infix fun String.astAssign(that: Any) = AstAssign(this, that.ast)
infix fun String.astAssignAdd(that: Any) = AstAssign(this, AstAdd(AstRead(this), that.ast))
infix fun String.astAssignSub(that: Any) = AstAssign(this, AstSubtract(AstRead(this), that.ast))
infix fun String.astAssignMul(that: Any) = AstAssign(this, AstMultiply(AstRead(this), that.ast))
infix fun String.astAssignMod(that: Any) = AstAssign(this, AstModulo(AstRead(this), that.ast))
infix fun String.astAssignDiv(that: Any) = AstAssign(this, AstDivide(AstRead(this), that.ast))

val Any.astReturnError get() = AstReturnError(ast)
val Any.astReturnValue get() = AstReturnValue(ast)
val AstExpression.astEval get() = AstEvaluate(this)

fun astInvokeOf(expression: Any) = AstEvaluate(expression.ast)

////////////////////
// Symbol helpers //
////////////////////

/**
 * Generates segment definition from the provided input parameters.
 */
fun astSegmentOf(
    module: String? = null,
    imports: List<String> = emptyList(),
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
    name: String = UUID.randomUUID().toString(),
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
    name: String = UUID.randomUUID().toString(),
    type: String,
    value: Any? = null,
    vis: Visibility = Visibility.PRIVATE,
    mut: Mutability = Mutability.IMMUTABLE,
    ass: Assignability = Assignability.CONSTANT,
) = AstProperty(
    name = name,
    type = type,
    value = value?.ast,
    visibility = vis,
    mutability = mut,
    assignability = ass,
)

/**
 * Generates variable definition from the provided input parameters.
 */
fun astVarOf(
    name: String = UUID.randomUUID().toString(),
    value: Any = 0,
    type: String? = null,
    vis: Visibility = Visibility.PRIVATE,
    mut: Mutability = Mutability.IMMUTABLE,
    ass: Assignability = Assignability.CONSTANT,
) = AstVariable(
    name = name,
    type = type,
    value = value.ast,
    visibility = vis,
    mutability = mut,
    assignability = ass,
)

/**
 * Generates function definition from the provided input parameters.
 */
fun astFunOf(
    name: String = UUID.randomUUID().toString(),
    valType: String? = null,
    errType: String? = null,
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
    name: String,
    type: String,
    value: Any? = null,
    pas: Passability = Passability.IN,
    ass: Assignability = Assignability.CONSTANT,
) = AstParameter(
    name = name,
    type = type,
    value = value?.ast,
    passability = pas,
    assignability = ass,
)

/**
 * Generates a branch statement from the provided input parameters.
 */
fun astIfOf(predicate: Any, success: List<AstStatement>, failure: List<AstStatement> = emptyList()) =
    AstBranch(predicate.ast, success, failure)

/**
 * Generates a branch expression from the provided input parameters.
 */
fun astWhenOf(expression: Any, vararg branches: Pair<Any, Any>, default: Any? = null) =
    AstWhen(expression.ast, branches.map { it.first.ast to it.second.ast }, default?.ast)
