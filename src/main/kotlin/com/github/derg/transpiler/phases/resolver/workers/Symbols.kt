package com.github.derg.transpiler.phases.resolver.workers

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
    private val node: HirDeclaration.ConstantDecl,
    private val env: Environment,
    scope: Scope,
) : Worker<Phase>
{
    private val worker = TypeExprResolver(node.kind, node.value, env, scope, true)
    
    override fun process(): Result<Phase, Outcome>
    {
        if (node.id !in env.declarations)
        {
            val type = worker.resolveDeclaration().valueOr { return it.toFailure() }
            env.declarations[node.id] = ThirDeclaration.Const(id = node.id, name = node.name, kind = type, def = null)
            return Phase.Declared.toSuccess()
        }
        
        val expr = worker.resolveDefinition().valueOr { return it.toFailure() }
        val value = Interpreter(env).evaluate(expr!!).valueOrDie()!!
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Const
        symbol.def = ThirDeclaration.ConstDef(value = value)
        return Phase.Defined.toSuccess()
    }
}

internal class FieldDefiner(
    private val node: HirDeclaration.FieldDecl,
    private val env: Environment,
    scope: Scope,
) : Worker<Phase>
{
    private val worker = TypeExprResolver(node.kind, node.default, env, scope, false)
    
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
    private val node: HirDeclaration.ParameterDecl,
    private val env: Environment,
    scope: Scope,
) : Worker<Phase>
{
    private val worker = TypeExprResolver(node.kind, node.default, env, scope, false)
    
    override fun process(): Result<Phase, Outcome>
    {
        if (node.id !in env.declarations)
        {
            val type = worker.resolveDeclaration().valueOr { return it.toFailure() }
            env.declarations[node.id] = ThirDeclaration.Parameter(id = node.id, name = node.name, passability = node.passability, kind = type, def = null)
            return Phase.Declared.toSuccess()
        }
        
        val value = worker.resolveDefinition().valueOr { return it.toFailure() }
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Parameter
        symbol.def = ThirDeclaration.ParameterDef(default = value)
        return Phase.Defined.toSuccess()
    }
}

internal class VariableDefiner(
    private val node: HirStatement.Variable,
    private val env: Environment,
    scope: Scope,
) : Worker<Phase>
{
    private val worker = TypeExprResolver(node.kind, node.value, env, scope, true)
    
    override fun process(): Result<Phase, Outcome>
    {
        if (node.id in env.declarations)
            return Phase.Defined.toSuccess()
        
        // When processing a variable, we are in the context of processing statements. Thus, we must ensure that the
        // variable is fully defined during this process.
        val type = worker.resolveDeclaration().valueOr { return it.toFailure() }
        val value = worker.resolveDefinition().valueOr { return it.toFailure() }
        env.declarations[node.id] = ThirDeclaration.Variable(id = node.id, name = node.name, kind = type, def = ThirDeclaration.VariableDef(value!!))
        return Phase.Declared.toSuccess()
    }
}

/**
 * Converts a HIR type parameter into a THIR declaration.
 */
internal class TypeParameterDefiner(
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
            kindWorker = node.kind.let { KindDefiner(it, env, scope) }
        if (kindOutcome == null)
            kindOutcome = kindWorker!!.process().valueOr { return it.toFailure() }
        
        if (node.id !in env.declarations)
        {
            env.declarations[node.id] = declare(kindOutcome!!)
            return Phase.Declared.toSuccess()
        }
        
        if (exprWorker == null)
            exprWorker = node.default?.let { expressionDefinerOf(it, env, scope, kindOutcome, true) }
        if (exprOutcome == null)
            exprOutcome = exprWorker?.process()?.valueOr { return it.toFailure() }
        
        val value = exprOutcome?.let { Interpreter(env).evaluate(it).valueOrDie()!! }
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
    private val node: HirDeclaration.FunctionDecl,
    private val env: Environment,
    parentScope: Scope,
) : Worker<Phase>
{
    // TODO: Create a new scope from the parent instead.
    val scope = parentScope.apply()
    {
        node.parameters.forEach { register(it.id, it.name) }
        node.typeParameters.forEach { register(it.id, it.name) }
    }
    
    private val valueKindWorker = KindDefiner(node.valueKind, env, scope)
    private val errorKindWorker = KindDefiner(node.errorKind, env, scope)
    private val statementWorker = WorkerList(node.body) { statementDefinerOf(it, env, scope) }
    
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
            val typeParameters = node.typeParameters.associate { it.id to TypeParameterDefiner(it, env, scope) }
            val parameters = node.parameters.associate { it.id to ParameterDefiner(it, env, scope) }
            val variables = node.body.filterIsInstance<HirStatement.Variable>().associate { it.id to VariableDefiner(it, env, scope) }
            val children = typeParameters + parameters + variables
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
    private val node: HirDeclaration.StructureDecl,
    private val env: Environment,
    parentScope: Scope,
) : Worker<Phase>
{
    // TODO: Create a new scope from the parent instead.
    val scope = parentScope.apply()
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
            val typeParameters = node.typeParameters.associate { it.id to TypeParameterDefiner(it, env, scope) }
            val fieldParameters = node.ctorEntries.filterIsInstance<HirDeclaration.FieldDecl>().associate { it.id to FieldDefiner(it, env, scope) }
            val fields = node.fields.associate { it.id to FieldDefiner(it, env, scope) }
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
 * Converts a HIR segment into a THIR declaration.
 */
internal class SegmentDefiner(
    private val node: HirDeclaration.SegmentDecl,
    private val env: Environment,
    parentScope: Scope,
) : Worker<Phase>
{
    // TODO: Create a new scope from the parent instead.
    val scope = parentScope.apply()
    {
        node.constants.forEach { register(it.id, it.name) }
        node.functions.forEach { register(it.id, it.name) }
        node.structures.forEach { register(it.id, it.name) }
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
        val constants = node.constants.associate { it.id to ConstDefiner(it, env, scope) }
        val functions = node.functions.associate { it.id to FunctionDefiner(it, env, scope) }
        val structures = node.structures.associate { it.id to StructureDefiner(it, env, scope) }
        return Phase.Spawn(constants + functions + structures).toSuccess()
    }
}
