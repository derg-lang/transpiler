package com.github.derg.transpiler.phases.resolver.workers

import com.github.derg.transpiler.phases.resolver.*
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

/**
 * Converts a HIR const into a THIR declaration.
 */
internal class ConstDefiner(
    private val node: HirDeclaration.ConstantDecl,
    private val env: Environment,
    private val scope: Scope,
) : Worker<Phase>
{
    private val typeWorker: Worker<ThirType>? = node.type?.let { typeDefinerOf(it, env, scope) }
    private var valueWorker: Worker<ThirExpression>? = null
    
    private var type: ThirType? = null
    private var value: ThirExpression? = null
    
    override fun process(): Result<Phase, Outcome>
    {
        // Constants are not required to have type information associated with them. When we have the type information,
        // however, we can declare the symbol before attempting to deduce the expression value of it. If we have enough
        // information to declare the symbol, we will do so immediately after deducing the type.
        if (type == null)
            type = typeWorker?.process()?.valueOr { return it.toFailure() }
        
        if (node.id !in env.declarations && type != null)
        {
            env.declarations[node.id] = declare(type!!)
            return Phase.Declared.toSuccess()
        }
        
        // Whether we have the type information or not, we need to resolve the expression as well. After the expression
        // has been resolved, we need to repeat the declaration step in case it did not happen due to missing type
        // information.
        if (valueWorker == null)
            valueWorker = expressionDefinerOf(node.value, env, scope, type, false)
        if (value == null)
            value = valueWorker!!.process().valueOr { return it.toFailure() }
        if (type == null)
        {
            // TODO: This verification logic is not correct. When the value type is void, we return an incorrect error.
            if (value!!.valueType == ThirType.Void)
                return Outcome.MismatchedType(expected = ThirType.Void, received = value!!.valueType).toFailure()
            type = value!!.valueType
        }
        
        if (node.id !in env.declarations)
        {
            env.declarations[node.id] = declare(type!!)
            return Phase.Declared.toSuccess()
        }
        
        // Once the symbol has been thoroughly declared, and we have the value of the symbol, we are ready to define it
        // and call it a day!
        if (value!!.valueType != type)
            return Outcome.MismatchedType(expected = type!!, received = value!!.valueType).toFailure()
        if (value!!.errorType != ThirType.Void)
            return Outcome.MismatchedType(expected = ThirType.Void, received = value!!.errorType).toFailure()
        
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Const
        symbol.def = ThirDeclaration.ConstDef(value = value!!)
        return Phase.Defined.toSuccess()
    }
    
    private fun declare(type: ThirType) = ThirDeclaration.Const(
        id = node.id,
        name = node.name,
        type = type,
        def = null,
    )
}

/**
 * Converts a HIR parameter into a THIR declaration.
 */
internal class ParameterDefiner(
    private val node: HirDeclaration.ParameterDecl,
    private val env: Environment,
    private val scope: Scope,
) : Worker<Phase>
{
    private val typeWorker: Worker<ThirType> = typeDefinerOf(node.type, env, scope)
    private var defaultWorker: Worker<ThirExpression>? = null
    
    private var type: ThirType? = null
    private var default: ThirExpression? = null
    
    override fun process(): Result<Phase, Outcome>
    {
        // Parameters cannot be declared without knowing their type and their default value. This information is crucial
        // when performing function overloading, so we cannot emit a declared parameter when default values are also
        // involved.
        if (type == null)
            type = typeWorker.process().valueOr { return it.toFailure() }
        if (defaultWorker == null && node.default != null)
            defaultWorker = expressionDefinerOf(node.default, env, scope, type, false)
        if (default == null)
            default = defaultWorker?.process()?.valueOr { return it.toFailure() }
        
        if (node.id !in env.declarations)
        {
            env.declarations[node.id] = declare(type!!)
            return Phase.Declared.toSuccess()
        }
        
        // Once the symbol has been thoroughly declared, and we have the value of the symbol, we are ready to define it
        // and call it a day!
        if (default != null)
        {
            if (default!!.valueType != type)
                return Outcome.MismatchedType(expected = type!!, received = default!!.valueType).toFailure()
            if (default!!.errorType != ThirType.Void)
                return Outcome.MismatchedType(expected = ThirType.Void, received = default!!.errorType).toFailure()
        }
        
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Parameter
        symbol.def = ThirDeclaration.ParameterDef(default = default)
        return Phase.Defined.toSuccess()
    }
    
    private fun declare(type: ThirType) = ThirDeclaration.Parameter(
        id = node.id,
        name = node.name,
        passability = node.passability,
        type = type,
        def = null,
    )
}

/**
 * Converts a HIR field into a THIR declaration.
 */
internal class FieldDefiner(
    private val node: HirDeclaration.FieldDecl,
    private val env: Environment,
    private val scope: Scope,
) : Worker<Phase>
{
    private val typeWorker: Worker<ThirType>? = node.type?.let { typeDefinerOf(it, env, scope) }
    private var valueWorker: Worker<ThirExpression>? = null
    
    private var type: ThirType? = null
    private var value: ThirExpression? = null
    
    override fun process(): Result<Phase, Outcome>
    {
        // Fields are not required to have type information associated with them. When we have the type information,
        // however, we can declare the symbol before attempting to deduce the expression value of it. If we have enough
        // information to declare the symbol, we will do so immediately after deducing the type.
        if (type == null)
            type = typeWorker?.process()?.valueOr { return it.toFailure() }
        
        if (node.id !in env.declarations && type != null)
        {
            env.declarations[node.id] = declare(type!!)
            return Phase.Declared.toSuccess()
        }
        
        // Whether we have the type information or not, we need to resolve the expression as well. After the expression
        // has been resolved, we need to repeat the declaration step in case it did not happen due to missing type
        // information.
        if (valueWorker == null)
            valueWorker = node.default?.let { expressionDefinerOf(it, env, scope, type, false) }
        if (value == null)
            value = valueWorker!!.process().valueOr { return it.toFailure() }
        if (type == null)
        {
            if (value!!.valueType == ThirType.Void)
                return Outcome.RequireType.toFailure()
            type = value!!.valueType
        }
        
        if (node.id !in env.declarations)
        {
            env.declarations[node.id] = declare(type!!)
            return Phase.Declared.toSuccess()
        }
        
        // Once the symbol has been thoroughly declared, and we have the value of the symbol, we are ready to define it
        // and call it a day!
        if (value!!.valueType != type)
            return Outcome.MismatchedType(expected = type!!, received = value!!.valueType).toFailure()
        if (value!!.errorType != ThirType.Void)
            return Outcome.MismatchedType(expected = ThirType.Void, received = value!!.errorType).toFailure()
        
        val symbol = env.declarations[node.id]!! as ThirDeclaration.Field
        symbol.def = ThirDeclaration.FieldDef(default = value!!)
        return Phase.Defined.toSuccess()
    }
    
    private fun declare(type: ThirType) = ThirDeclaration.Field(
        id = node.id,
        name = node.name,
        type = type,
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
    }
    
    private val valueTypeWorker = node.valueType?.let { typeDefinerOf(it, env, scope) }
    private val errorTypeWorker = node.errorType?.let { typeDefinerOf(it, env, scope) }
    private val statementWorker = WorkerList(node.body) { statementDefinerOf(it, env, scope) }
    
    private var valueType: ThirType? = null
    private var errorType: ThirType? = null
    private var statements: List<ThirStatement>? = null
    
    private var hasSpawnedChildren = false
    
    override fun process(): Result<Phase, Outcome>
    {
        // Functions which have a non-zero number of parameters must ensure that all the parameters are worked on as
        // well. We spawn new workers for each and every parameter, letting them all run in parallel.
        if (!hasSpawnedChildren)
        {
            hasSpawnedChildren = true
            val parameters = node.parameters.associate { it.id to ParameterDefiner(it, env, scope) }
            val variables = node.body.filterIsInstance<HirStatement.Variable>().associate { it.id to VariableDefiner(it, env, scope) }
            val children = parameters + variables
            if (children.isNotEmpty())
                return Phase.Spawn(parameters + variables).toSuccess()
        }
        
        // Function declarations depend primarily on their value and error types. The parameters are crucial as well,
        // although they are treated as their own resource and resolved independently. Thus, to declare the function all
        // we need is the type information.
        if (valueType == null)
            valueType = valueTypeWorker?.process()?.valueOr { return it.toFailure() } ?: ThirType.Void
        if (errorType == null)
            errorType = errorTypeWorker?.process()?.valueOr { return it.toFailure() } ?: ThirType.Void
        
        if (node.id !in env.declarations)
        {
            env.declarations[node.id] = declare(valueType, errorType)
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
    
    private fun declare(valueType: ThirType?, errorType: ThirType?) = ThirDeclaration.Function(
        id = node.id,
        name = node.name,
        valueType = valueType!!,
        errorType = errorType!!,
        genericTypeIds = emptyList(),
        genericValueIds = emptyList(),
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
    }
    
    private var hasSpawnedChildren = false
    
    override fun process(): Result<Phase, Outcome>
    {
        // Structures which have a non-zero number of fields must ensure that all the fields are worked on as well. We
        // spawn new workers for each and every field, letting them all run in parallel.
        if (!hasSpawnedChildren)
        {
            hasSpawnedChildren = true
            val fields = node.fields.associate { it.id to FieldDefiner(it, env, scope) }
            if (fields.isNotEmpty())
                return Phase.Spawn(fields).toSuccess()
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
        genericTypeIds = emptyList(),
        genericValueIds = emptyList(),
        fieldIds = node.fields.map { it.id },
        def = null,
    )
}

/**
 * Converts from a HIR variable to a THIR declaration.
 */
internal class VariableDefiner(
    private val node: HirStatement.Variable,
    private val env: Environment,
    private val scope: Scope,
) : Worker<Phase>
{
    private val typeWorker: Worker<ThirType>? = node.type?.let { typeDefinerOf(it, env, scope) }
    private var valueWorker: Worker<ThirExpression>? = null
    
    private var type: ThirType? = null
    private var value: ThirExpression? = null
    
    override fun process(): Result<Phase, Outcome>
    {
        // When processing a variable, we are in the context of processing statements. Thus, we must ensure that the
        // variable is fully defined during this process.
        if (type == null)
            type = typeWorker?.process()?.valueOr { return it.toFailure() }
        if (valueWorker == null)
            valueWorker = expressionDefinerOf(node.value, env, scope, type, true)
        if (value == null)
            value = valueWorker!!.process().valueOr { return it.toFailure() }
        if (type == null)
        {
            // TODO: This verification logic is not correct. When the value type is void, we return an incorrect error.
            if (value!!.valueType == ThirType.Void)
                return Outcome.MismatchedType(expected = ThirType.Void, received = value!!.valueType).toFailure()
            type = value!!.valueType
        }
        
        // Once all the resolution is done, we can immediately define the symbol. After that is done, we emit a single
        // instruction for assigning the value to this variable.
        if (value!!.valueType != type)
            return Outcome.MismatchedType(expected = type!!, received = value!!.valueType).toFailure()
        if (value!!.errorType != ThirType.Void)
            return Outcome.MismatchedType(expected = ThirType.Void, received = value!!.errorType).toFailure()
        
        if (node.id !in env.declarations)
        {
            env.declarations[node.id] = declare(type!!, value!!)
            return Phase.Declared.toSuccess()
        }
        
        return Phase.Defined.toSuccess()
    }
    
    private fun declare(type: ThirType, value: ThirExpression) = ThirDeclaration.Variable(
        id = node.id,
        name = node.name,
        type = type,
        def = ThirDeclaration.VariableDef(value),
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
