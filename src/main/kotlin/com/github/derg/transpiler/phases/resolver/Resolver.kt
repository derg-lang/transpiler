package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.Id
import com.github.derg.transpiler.source.IdProviderSystem
import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.hir.Function
import com.github.derg.transpiler.util.*

/**
 * Resolves all provided [segments] into a single package with the given [name]. The resolver phase ensures that all
 * symbols are properly linked together, converting all identifiers into an unambiguous id representing the symbol
 * itself. This phase ensures the source code is valid from a type safety perspective.
 */
fun resolve(name: Name, segments: List<Segment>): Package =
    Resolver(Builtin.SYMBOLS).resolve(name, segments).valueOr { throw IllegalStateException("Failed resolving: $it") }

/**
 * During the resolving phase, various types of errors may be encountered. These errors are typically related to type
 * errors, as type checking takes place in this phase.
 */
sealed interface ResolveError
{
    /**
     * The callable object with the given [name] is not recognized and did not resolve to anything which could be
     * invoked.
     */
    data class UnknownFunction(val name: Name) : ResolveError
    
    /**
     * The literal [name] is not recognized and cannot be used to convert the constant into a sensible value.
     */
    data class UnknownLiteral(val name: Name) : ResolveError
    
    /**
     * The type with the given [name] could not be found in the current or any outer scope.
     */
    data class UnknownType(val name: Name) : ResolveError
    
    /**
     * The variable with the given [name] could not be found in the current or any outer scope.
     */
    data class UnknownVariable(val name: Name) : ResolveError
    
    /**
     * No function candidates were found for the function with the given [name], when invoked with the given
     * [parameters].
     */
    data class MismatchedFunctionTypes(val name: Name, val parameters: List<Type>) : ResolveError
    
    /**
     * During type resolution, a parameter was resolved to the [expected] type, but the default value provided for the
     * parameter resolved to the [actual] type instead.
     */
    data class MismatchedParameterType(val expected: Type, val actual: Type) : ResolveError
    
    /**
     * During type resolution, a variable was resolved to the [expected] type, but the explicitly provided type for the
     * variable resolved to the [actual] type instead.
     */
    data class MismatchedVariableType(val expected: Type, val actual: Type) : ResolveError
    
    /**
     * The provided [type] is not permitted for usage as a predicate in a branch statement.
     */
    data class InvalidPredicateType(val type: Type) : ResolveError
}

/**
 * TODO: Highly in-progress and untested code, but it will do for now! Make sure to re-visit this part of the codebase
 *       at some point not too far into the future and actually clean up whatever is going on in here.
 */
class Resolver(private val symbols: SymbolTable)
{
    /**
     * Constructs a package with the given [name] from the collection of [segments]. Every segment may refer to zero or
     * one module; all modules will be compiled in the appropriate order.
     */
    fun resolve(name: Name, segments: List<Segment>): Result<Package, ResolveError>
    {
        // May now proceed with figuring out how to structure the package!
        val `package` = Package(Id.randomUUID(), name, SymbolTable(symbols))
        val groups = segments.groupBy { it.module }
        
        // The order in which modules are resolved depends on their dependency graph. Modules cannot be permitted to
        // have circular dependencies, as they may import symbols from each other. In order to resolve a module, all its
        // dependencies must be known ahead of time, forcing a specific evaluation order.
        // TODO: Implement me!
        // TODO: Introduce the names of all external dependencies when the time is right.
        // TODO: Should all segments be ordered in any particular order?
        
        // With all modules sorted in the appropriate order, they may be processed in that very same ordering
        for ((moduleName, groupedSegments) in groups.entries)
        {
            // TODO: Sort all instructions in the order in which they can be initialized. How can we determine in which
            //       order all instructions should be executed? Try to execute them and just try to find an ordering
            //       that "just works"? Cannot allow side-effect functions to be invoked in that case.
            // TODO: Figure out a suitable default name for modules
            val module = Module(Id.randomUUID(), moduleName ?: "__main").also { `package`.symbols.register(it) }
            val statements = groupedSegments.flatMap { it.definitions }
            
            module.scope = `package`.symbols.resolveScope(statements).valueOr { return failureOf(it) }
        }
        return `package`.toSuccess()
    }
}

/**
 * Converts the given [name] into a type which has it as the name. Any other symbol with the same name, but are not a
 * type, will be ignored. If the name is not provided, [Builtin.VOID] is returned instead.
 */
internal fun SymbolTable.resolveOptionalType(name: Name?): Result<Type, ResolveError>
{
    if (name == null)
        return Builtin.VOID.toSuccess()
    
    val candidate = find(name).filterIsInstance<Type>().firstOrNull()
    return candidate?.toSuccess() ?: ResolveError.UnknownType(name).toFailure()
}

/**
 * Converts the given [expression] into a value which represents the same expression. If no expression was provided, an
 * empty value is returned instead.
 */
internal fun SymbolTable.resolveOptionalValue(expression: Expression?): Result<Value?, ResolveError> =
    if (expression == null) successOf(null) else resolveRequiredValue(expression)

/**
 * Converts the given [name] into a function which is compatible with the provided [parameters].
 */
internal fun SymbolTable.resolveRequiredFunction(name: Name, parameters: List<Type>): Result<Function, ResolveError>
{
    // TODO: Support variable callables as well
    val functions = find(name).filterIsInstance<Function>()
    if (functions.isEmpty())
        return ResolveError.UnknownFunction(name).toFailure()
    
    val candidates = functions.filter { it.isCompatibleWith(parameters) }
    return candidates.firstOrNull()?.toSuccess() ?: ResolveError.MismatchedFunctionTypes(name, parameters).toFailure()
}

private fun Function.isCompatibleWith(parameters: List<Type>): Boolean
{
    if (this.params.size != parameters.size)
        return false
    return this.params.zip(parameters).all { it.first.type.id == it.second.id }
}

/**
 * Converts the given [statement] into an instruction which when executed performs the statement action.
 */
internal fun SymbolTable.resolveStatement(statement: Statement): Result<Instruction, ResolveError> = when (statement)
{
    is Assignment.Assign   -> ConverterAssign(this)(statement)
    is Control.Branch      -> ConverterBranch(this)(statement)
    is Control.Invoke      -> TODO()
    is Control.Raise       -> ConverterRaise(this)(statement)
    is Control.Return      -> ConverterReturn(this)(statement)
    is Definition.Function -> Nop.toSuccess()
    is Definition.Type     -> Nop.toSuccess()
    is Definition.Variable -> ConverterAssign(this)(Assignment.Assign(statement.name, statement.value))
}

/**
 * Converts the given [expression] into a value which represents the same expression.
 */
internal fun SymbolTable.resolveRequiredValue(expression: Expression): Result<Value, ResolveError> = when (expression)
{
    is Access.Function       -> ConverterInvoke(this)(expression)
    is Access.Subscript      -> TODO()
    is Access.Variable       -> ConverterRead(this)(expression)
    is Constant.Bool         -> ConverterBool(expression)
    is Constant.Real         -> ConverterReal(this)(expression)
    is Constant.Text         -> ConverterText(this)(expression)
    is Operator.Add          -> ConverterAdd(this)(expression)
    is Operator.And          -> ConverterAnd(this)(expression)
    is Operator.Catch        -> TODO()
    is Operator.Divide       -> ConverterDivide(this)(expression)
    is Operator.Equal        -> ConverterEqual(this)(expression)
    is Operator.Greater      -> ConverterGreater(this)(expression)
    is Operator.GreaterEqual -> ConverterGreaterEqual(this)(expression)
    is Operator.Less         -> ConverterLess(this)(expression)
    is Operator.LessEqual    -> ConverterLessEqual(this)(expression)
    is Operator.Minus        -> ConverterUnaryMinus(this)(expression)
    is Operator.Modulo       -> ConverterModulo(this)(expression)
    is Operator.Multiply     -> ConverterMultiply(this)(expression)
    is Operator.Not          -> ConverterNot(this)(expression)
    is Operator.NotEqual     -> ConverterNotEqual(this)(expression)
    is Operator.Or           -> ConverterOr(this)(expression)
    is Operator.Plus         -> ConverterUnaryPlus(this)(expression)
    is Operator.Raise        -> TODO()
    is Operator.Subtract     -> ConverterSubtract(this)(expression)
    is Operator.ThreeWay     -> TODO()
    is Operator.Xor          -> ConverterXor(this)(expression)
    is When                  -> TODO()
}

/**
 * Converts the given [name] into a variable.
 */
internal fun SymbolTable.resolveRequiredVariable(name: Name): Result<Variable, ResolveError>
{
    // TODO: Support function variables as well
    val candidate = find(name).filterIsInstance<Variable>().firstOrNull()
    return candidate?.toSuccess() ?: ResolveError.UnknownVariable(name).toFailure()
}

/**
 * Converts all the provided [statements] into instructions, baked into a new scope.
 */
internal fun SymbolTable.resolveScope(statements: List<Statement>): Result<Scope, ResolveError>
{
    val inner = SymbolTable(this)
    
    // In order to perform type checking, all statements must be declared up-front. Once the declaration has taken
    // place, we may define the symbols and use them where appropriate.
    val context = Declarator(inner, IdProviderSystem)(statements).valueOr { return failureOf(it) }
    
    context.functions.fold { DefinerFunction(inner)(it.first, it.second) }.onFailure { return failureOf(it) }
    context.types.fold { DefinerType(inner)(it.first, it.second) }.onFailure { return failureOf(it) }
    
    // Once all setup is completed, we may finally convert the statements into actually executable code.
    val instructions = statements.fold { inner.resolveStatement(it) }.valueOr { return failureOf(it) }
    return Scope(instructions.filterNot { it == Nop }, inner).toSuccess()
}
