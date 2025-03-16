package com.github.derg.transpiler.source.ast

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.utils.*
import java.util.*

/////////////////////
// Literal helpers //
/////////////////////

val Any.ast: AstValue
    get() = when (this)
    {
        is AstValue -> this
        is Boolean  -> AstBool(this)
        is Int      -> AstInteger(toBigInteger(), INT32_LIT_NAME)
        is Long     -> AstInteger(toBigInteger(), INT64_LIT_NAME)
        is String   -> AstText(this, STR_LIT_NAME)
        else        -> throw IllegalArgumentException("Value $this does not represent a valid ast value")
    }

//////////////////
// Type helpers //
//////////////////

fun astTemplateStruct(
    name: String = UUID.randomUUID().toString(),
) = AstTemplate.Type(name = name)

fun astTemplateValue(
    name: String = UUID.randomUUID().toString(),
    type: AstType,
    default: AstValue? = null,
) = AstTemplate.Value(name = name, type = type, default = default)

fun astTypeData(
    name: String = UUID.randomUUID().toString(),
    mutability: Mutability = Mutability.IMMUTABLE,
    parameters: List<AstParameterStatic> = emptyList(),
) = AstType.Structure(name = name, mutability = mutability, parameters = parameters)

fun astParamStatic(
    name: String? = null,
    value: AstValue = 0.ast,
) = AstParameterStatic(name = name, value = value)

////////////////////////
// Expression helpers //
////////////////////////

val Any.astNot get() = AstNot(ast)
val Any.astPlus get() = AstPlus(ast)
val Any.astMinus get() = AstMinus(ast)

infix fun Any.astEq(that: Any) = AstEqual(this.ast, that.ast)
infix fun Any.astNe(that: Any) = AstNotEqual(this.ast, that.ast)
infix fun Any.astGe(that: Any) = AstGreaterEqual(this.ast, that.ast)
infix fun Any.astGt(that: Any) = AstGreater(this.ast, that.ast)
infix fun Any.astLe(that: Any) = AstLessEqual(this.ast, that.ast)
infix fun Any.astLt(that: Any) = AstLess(this.ast, that.ast)
infix fun Any.astTw(that: Any) = AstThreeWay(this.ast, that.ast)

infix fun Any.astAdd(that: Any) = AstAdd(this.ast, that.ast)
infix fun Any.astSub(that: Any) = AstSubtract(this.ast, that.ast)
infix fun Any.astMul(that: Any) = AstMultiply(this.ast, that.ast)
infix fun Any.astDiv(that: Any) = AstDivide(this.ast, that.ast)
infix fun Any.astMod(that: Any) = AstModulo(this.ast, that.ast)

infix fun Any.astAnd(that: Any) = AstAnd(this.ast, that.ast)
infix fun Any.astOr(that: Any) = AstOr(this.ast, that.ast)
infix fun Any.astXor(that: Any) = AstXor(this.ast, that.ast)

infix fun Any.astCatchRaise(that: Any) = AstCatch(this.ast, that.ast, Capture.RAISE)
infix fun Any.astCatchReturn(that: Any) = AstCatch(this.ast, that.ast, Capture.RETURN)
infix fun Any.astCatchHandle(that: Any) = AstCatch(this.ast, that.ast, Capture.HANDLE)

fun String.astLoad(vararg parameters: Any) = AstLoad(this, parameters.map { it.astArg })
fun AstValue.astCall(vararg parameters: Any) = AstCall(this, parameters.map { it.astArg })
fun AstValue.astMember(field: String) = AstMember(this, field.astLoad())

val Any.astArg: NamedMaybe<AstValue>
    get() = if (this is Pair<*, *>) (first as String) to (second as Any).ast else null to ast

///////////////////////
// Statement helpers //
///////////////////////

infix fun String.astAssign(that: Any) = AstAssign(this, that.ast)
infix fun String.astAssignAdd(that: Any) = AstAssignAdd(this, that.ast)
infix fun String.astAssignSub(that: Any) = AstAssignSubtract(this, that.ast)
infix fun String.astAssignMul(that: Any) = AstAssignMultiply(this, that.ast)
infix fun String.astAssignMod(that: Any) = AstAssignModulo(this, that.ast)
infix fun String.astAssignDiv(that: Any) = AstAssignDivide(this, that.ast)

val Any.astReturnError get() = AstReturnError(ast)
val Any.astReturnValue get() = AstReturnValue(ast)
val Any.astEval get() = AstEvaluate(ast)

fun astInvokeOf(expression: Any) = AstEvaluate(expression.ast)

//////////////////////
// Unsorted helpers //
//////////////////////

/**
 * Generates segment definition from the provided input parameters.
 */
fun astSegmentOf(
    imports: List<String> = emptyList(),
    statements: List<AstSymbol> = emptyList(),
) = AstSegment(
    imports = imports,
    definitions = statements,
)

/**
 * Generates a branch statement from the provided input parameters.
 */
fun astIfOf(predicate: Any, success: List<AstInstruction>, failure: List<AstInstruction> = emptyList()) =
    AstBranch(predicate.ast, success, failure)

/**
 * Generates a for loop statement from the provided input parameters.
 */
fun astForOf(
    identifier: String = UUID.randomUUID().toString(),
    expression: Any,
    instructions: List<AstInstruction> = emptyList(),
) = AstFor(identifier, expression.ast, instructions)

/**
 * Generates a when expression from the provided input parameters.
 */
fun astWhenOf(expression: Any, vararg branches: Pair<Any, Any>, default: Any? = null) =
    AstWhen(expression.ast, branches.map { it.first.ast to it.second.ast }, default?.ast)

/**
 * Generates a while loop statement from the provided input parameters.
 */
fun astWhileOf(
    expression: Any,
    instructions: List<AstInstruction> = emptyList(),
) = AstWhile(expression.ast, instructions)

////////////////////
// Symbol helpers //
////////////////////

fun astConstOf(
    name: String = UUID.randomUUID().toString(),
    type: String = INT32_TYPE_NAME,
    value: Any = 0,
    vis: Visibility = Visibility.PRIVATE,
) = AstConstant(
    name = name,
    type = AstType.Structure(type, Mutability.IMMUTABLE, emptyList()),
    value = value.ast,
    visibility = vis,
)

fun astFunOf(
    name: String = UUID.randomUUID().toString(),
    valType: String? = null,
    errType: String? = null,
    vis: Visibility = Visibility.PRIVATE,
    params: List<AstParameter> = emptyList(),
    statements: List<AstInstruction> = emptyList(),
) = AstFunction(
    name = name,
    valueType = valType?.let { AstType.Structure(it, Mutability.IMMUTABLE, emptyList()) },
    errorType = errType?.let { AstType.Structure(it, Mutability.IMMUTABLE, emptyList()) },
    parameters = params,
    visibility = vis,
    statements = statements,
)

fun astParOf(
    name: String,
    type: String,
    value: Any? = null,
    pas: Passability = Passability.IN,
    mut: Mutability = Mutability.IMMUTABLE,
) = AstParameter(
    name = name,
    type = AstType.Structure(type, mut, emptyList()),
    value = value?.ast,
    passability = pas,
)

fun astPropOf(
    name: String = UUID.randomUUID().toString(),
    type: String,
    value: Any? = null,
    vis: Visibility = Visibility.PRIVATE,
    mut: Mutability = Mutability.IMMUTABLE,
    ass: Assignability = Assignability.FINAL,
) = AstProperty(
    name = name,
    type = AstType.Structure(type, mut, emptyList()),
    value = value?.ast,
    visibility = vis,
    assignability = ass,
)

fun astStructOf(
    name: String = UUID.randomUUID().toString(),
    vis: Visibility = Visibility.PRIVATE,
    props: List<AstProperty> = emptyList(),
    templates: List<AstTemplate> = emptyList(),
) = AstStruct(
    name = name,
    visibility = vis,
    fields = props,
    templates = templates,
)

fun astVarOf(
    name: String = UUID.randomUUID().toString(),
    value: Any = 0,
    type: String? = null,
    vis: Visibility = Visibility.PRIVATE,
    mut: Mutability = Mutability.IMMUTABLE,
    ass: Assignability = Assignability.FINAL,
) = AstVariable(
    name = name,
    type = type?.let { AstType.Structure(it, mut, emptyList()) },
    value = value.ast,
    visibility = vis,
    assignability = ass,
)
