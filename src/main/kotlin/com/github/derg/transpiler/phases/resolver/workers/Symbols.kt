package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.interpreter.*
import com.github.derg.transpiler.phases.resolver.*
import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.thir.*
import com.github.derg.transpiler.utils.*
import java.util.*

/**
 * The state indicates where in the processing chain the symbol is. All symbols must go through both states, first it
 * must be declared, then it must be defined. A symbol which is declared may be used by other nodes for type checking
 * purposes, but to interpret code based on the symbol, it must be defined.
 *
 * At any point, a worker may determine that one or more child workers must be spawned. All workers operate
 * independently of each other, but may depend on each other depending on which phase each worker is in. Note that only
 * workers working on declarations are permitted to spawn additional workers.
 */
internal sealed interface Phase
{
    data class Spawn(val workers: Map<UUID, Worker<Phase>>) : Phase
    data object Declared : Phase
    data object Defined : Phase
}

internal class ConstDefiner(
    private val evaluator: Evaluator,
    private val node: HirDeclaration.ConstantDecl,
    private val env: Environment,
    scope: Scope,
    private val globals: StackFrame,
) : Worker<Phase>
{
    private val worker = TypeExprResolver(evaluator, node.kind, node.value, env, scope, true)
    
    override fun process(): Result<Phase, Outcome>
    {
        if (node.id !in env.declarations)
        {
            val type = worker.resolveDeclaration().valueOr { return it.toFailure() }
            env.declarations[node.id] = ThirDeclaration.Const(id = node.id, name = node.name, kind = type, def = null)
            return Phase.Declared.toSuccess()
        }
        
        val expr = worker.resolveDefinition().valueOr { return it.toFailure() }
        val value = evaluator.evaluate(expr!!).valueOrDie()!!
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Const
        symbol.def = ThirDeclaration.ConstDef(value = value)
        globals[symbol.id] = value
        return Phase.Defined.toSuccess()
    }
}

internal class FieldDefiner(
    evaluator: Evaluator,
    private val node: HirDeclaration.FieldDecl,
    private val env: Environment,
    scope: Scope,
) : Worker<Phase>
{
    private val worker = TypeExprResolver(evaluator, node.kind, node.default, env, scope, false)
    
    override fun process(): Result<Phase, Outcome>
    {
        if (node.id !in env.declarations)
        {
            val type = worker.resolveDeclaration().valueOr { return it.toFailure() }
            env.declarations[node.id] = ThirDeclaration.Field(id = node.id, name = node.name, kind = type, def = null)
            return Phase.Declared.toSuccess()
        }
        
        val value = worker.resolveDefinition().valueOr { return it.toFailure() }
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Field
        symbol.def = ThirDeclaration.FieldDef(default = value)
        return Phase.Defined.toSuccess()
    }
}

internal class ParameterDefiner(
    evaluator: Evaluator,
    private val node: HirDeclaration.ParameterDecl,
    private val env: Environment,
    scope: Scope,
) : Worker<Phase>
{
    private val worker = TypeExprResolver(evaluator, node.kind, node.default, env, scope, false)
    
    override fun process(): Result<Phase, Outcome>
    {
        if (node.id !in env.declarations)
        {
            val type = worker.resolveDeclaration().valueOr { return it.toFailure() }
            env.declarations[node.id] = ThirDeclaration.Parameter(node.id, node.name, node.passability, type, null)
            return Phase.Declared.toSuccess()
        }
        
        val value = worker.resolveDefinition().valueOr { return it.toFailure() }
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Parameter
        symbol.def = ThirDeclaration.ParameterDef(default = value)
        return Phase.Defined.toSuccess()
    }
}

/**
 * Converts a HIR type parameter into a THIR declaration.
 */
internal class TypeParameterDefiner(
    private val evaluator: Evaluator,
    private val node: HirDeclaration.TypeParameterDecl,
    private val env: Environment,
    private val scope: Scope,
) : Worker<Phase>
{
    private var kindWorker: Worker<ThirKind>? = null
    private var kindOutcome: ThirKind? = null
    private var exprWorker: Worker<ThirExpression>? = null
    private var exprOutcome: ThirExpression? = null
    
    override fun process(): Result<Phase, Outcome>
    {
        if (kindWorker == null)
            kindWorker = node.kind.let { KindDefiner(evaluator, it, env, scope) }
        if (kindOutcome == null)
            kindOutcome = kindWorker!!.process().valueOr { return it.toFailure() }
        
        if (node.id !in env.declarations)
        {
            env.declarations[node.id] = declare(kindOutcome!!)
            return Phase.Declared.toSuccess()
        }
        
        if (exprWorker == null)
            exprWorker = node.default?.let { expressionDefinerOf(evaluator, it, env, scope, kindOutcome, true) }
        if (exprOutcome == null)
            exprOutcome = exprWorker?.process()?.valueOr { return it.toFailure() }
        
        val value = exprOutcome?.let { evaluator.evaluate(it).valueOrDie()!! }
        val symbol = env.declarations[node.id]!! as ThirDeclaration.TypeParameter
        symbol.def = ThirDeclaration.TypeParameterDef(default = value)
        return Phase.Defined.toSuccess()
    }
    
    private fun declare(kind: ThirKind) = ThirDeclaration.TypeParameter(
        id = node.id,
        name = node.name,
        kind = kind,
        def = null,
    )
}

/**
 * Converts a HIR function into a THIR declaration.
 */
internal class FunctionDefiner(
    private val evaluator: Evaluator,
    private val node: HirDeclaration.FunctionDecl,
    private val env: Environment,
    parentScope: Scope,
) : Worker<Phase>
{
    val scope = Scope(parentScope).apply()
    {
        node.parameters.forEach { register(it.id, it.name) }
        node.typeParameters.forEach { register(it.id, it.name) }
    }
    
    private val valueKindWorker = KindDefiner(evaluator, node.valueKind, env, scope)
    private val errorKindWorker = KindDefiner(evaluator, node.errorKind, env, scope)
    private val statementWorker = WorkerList(node.body) { statementDefinerOf(evaluator, it, env, scope) }
    
    private var valueKind: ThirKind? = null
    private var errorKind: ThirKind? = null
    private var statements: List<ThirStatement>? = null
    
    private var hasSpawnedChildren = false
    
    override fun process(): Result<Phase, Outcome>
    {
        // Functions which have a non-zero number of parameters must ensure that all the parameters are worked on as
        // well. We spawn new workers for each and every parameter, letting them all run in parallel.
        if (!hasSpawnedChildren)
        {
            hasSpawnedChildren = true
            val typeParameters = node.typeParameters.associate { it.id to TypeParameterDefiner(evaluator, it, env, scope) }
            val parameters = node.parameters.associate { it.id to ParameterDefiner(evaluator, it, env, scope) }
            val children = typeParameters + parameters
            if (children.isNotEmpty())
                return Phase.Spawn(children).toSuccess()
        }
        
        // Function declarations depend primarily on their value and error types. The parameters are crucial as well,
        // although they are treated as their own resource and resolved independently. Thus, to declare the function all
        // we need is the type information.
        if (valueKind == null)
            valueKind = valueKindWorker.process().valueOr { return it.toFailure() }
        if (errorKind == null)
            errorKind = errorKindWorker.process().valueOr { return it.toFailure() }
        
        if (node.id !in env.declarations)
        {
            env.declarations[node.id] = declare(valueKind, errorKind)
            return Phase.Declared.toSuccess()
        }
        
        // Once the function is fully declared, we can process the statements. Statements must be resolved after the
        // type of the function has been determined, enabling us recursion and self-reference within the statements.
        if (statements == null)
            statements = statementWorker.process().valueOr { return it.toFailure() }
        
        // TODO: Perform type-checking on all statements. We need to enforce that all return statements only returns
        //       types which are compatible with the function's value and error types. This is also where we should
        //       perform smart-casting where possible, reducing user friction.
        
        // Once the symbol has been thoroughly declared, and we have all statements, we are ready to define it and call
        // it a day!
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Function
        symbol.def = ThirDeclaration.FunctionDef(statements = statements!!)
        return Phase.Defined.toSuccess()
    }
    
    private fun declare(valueKind: ThirKind?, errorKind: ThirKind?) = ThirDeclaration.Function(
        id = node.id,
        name = node.name,
        valueKind = valueKind ?: ThirKind.Nothing,
        errorKind = errorKind ?: ThirKind.Nothing,
        typeParameterIds = node.typeParameters.map { it.id },
        parameterIds = node.parameters.map { it.id },
        def = null,
    )
}

/**
 * Converts from a HIR structure to a THIR declaration.
 */
internal class StructureDefiner(
    private val evaluator: Evaluator,
    private val node: HirDeclaration.StructureDecl,
    private val env: Environment,
    parentScope: Scope,
) : Worker<Phase>
{
    val scope = Scope(parentScope).apply()
    {
        node.fields.forEach { register(it.id, it.name) }
        node.ctorEntries.forEach { register(it.id, it.name) }
        node.typeParameters.forEach { register(it.id, it.name) }
    }
    
    private var hasSpawnedChildren = false
    
    override fun process(): Result<Phase, Outcome>
    {
        // Structures which have a non-zero number of fields must ensure that all the fields are worked on as well. We
        // spawn new workers for each and every field, letting them all run in parallel.
        if (!hasSpawnedChildren)
        {
            hasSpawnedChildren = true
            val typeParameters = node.typeParameters.associate { it.id to TypeParameterDefiner(evaluator, it, env, scope) }
            val fieldParameters = node.ctorEntries.filterIsInstance<HirDeclaration.FieldDecl>().associate { it.id to FieldDefiner(evaluator, it, env, scope) }
            val fields = node.fields.associate { it.id to FieldDefiner(evaluator, it, env, scope) }
            val children = typeParameters + fieldParameters + fields
            if (children.isNotEmpty())
                return Phase.Spawn(children).toSuccess()
        }
        
        if (node.id !in env.declarations)
        {
            env.declarations[node.id] = declare()
            return Phase.Declared.toSuccess()
        }
        
        // Once the symbol has been thoroughly declared, and we have all statements, we are ready to define it and call
        // it a day!
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Structure
        symbol.def = ThirDeclaration.StructureDef(placeholder = null)
        return Phase.Defined.toSuccess()
    }
    
    private fun declare() = ThirDeclaration.Structure(
        id = node.id,
        name = node.name,
        typeParameterIds = emptyList(),
        ctorEntryIds = node.ctorEntries.map { it.id },
        fieldIds = node.ctorEntries.filterIsInstance<HirDeclaration.FieldDecl>().map { it.id } + node.fields.map { it.id },
        def = null,
    )
}

/**
 * Converts a HIR module into a THIR declaration.
 */
internal class ModuleDefiner(
    private val evaluator: Evaluator,
    private val node: HirDeclaration.ModuleDecl,
    private val env: Environment,
    private val externalScope: Scope,
    private val packageScope: Scope,
    private val globals: StackFrame,
) : Worker<Phase>
{
    private val moduleScope = Scope(packageScope)
    private var hasSpawnedChildren = false
    private var hasDeclaredItself = false
    
    override fun process(): Result<Phase, Outcome>
    {
        if (hasDeclaredItself)
            return Phase.Defined.toSuccess()
        if (hasSpawnedChildren)
        {
            hasDeclaredItself = true
            return Phase.Declared.toSuccess()
        }
        
        hasSpawnedChildren = true
        val segments = node.segments.associate { it.id to SegmentDefiner(evaluator, it, env, externalScope, packageScope, moduleScope, globals) }
        return Phase.Spawn(segments).toSuccess()
    }
}

/**
 * Converts a HIR segment into a THIR declaration.
 */
internal class SegmentDefiner(
    private val evaluator: Evaluator,
    private val node: HirDeclaration.SegmentDecl,
    private val env: Environment,
    externalScope: Scope,
    packageScope: Scope,
    moduleScope: Scope,
    private val globals: StackFrame,
) : Worker<Phase>
{
    private val segmentScope = Scope(moduleScope)
    
    init
    {
        // All declarations must be registered into their corresponding scopes based on visibility.
        val symbols = node.constants.map { it.visibility to it } +
                node.functions.map { it.visibility to it } +
                node.structures.map { it.visibility to it }
        
        val declarations = symbols.groupBy { it.first }.mapValues { it.value.map { foo -> foo.second } }
        declarations[Visibility.PRIVATE]?.forEach { segmentScope.register(it.id, it.name) }
        declarations[Visibility.PROTECTED]?.forEach { moduleScope.register(it.id, it.name) }
        declarations[Visibility.PUBLIC]?.forEach { packageScope.register(it.id, it.name) }
        declarations[Visibility.EXPORTED]?.forEach { externalScope.register(it.id, it.name) }
    }
    
    private var hasSpawnedChildren = false
    private var hasDeclaredItself = false
    
    override fun process(): Result<Phase, Outcome>
    {
        if (hasDeclaredItself)
            return Phase.Defined.toSuccess()
        if (hasSpawnedChildren)
        {
            hasDeclaredItself = true
            return Phase.Declared.toSuccess()
        }
        
        hasSpawnedChildren = true
        val constants = node.constants.associate { it.id to ConstDefiner(evaluator, it, env, segmentScope, globals) }
        val functions = node.functions.associate { it.id to FunctionDefiner(evaluator, it, env, segmentScope) }
        val structures = node.structures.associate { it.id to StructureDefiner(evaluator, it, env, segmentScope) }
        return Phase.Spawn(constants + functions + structures).toSuccess()
    }
}
