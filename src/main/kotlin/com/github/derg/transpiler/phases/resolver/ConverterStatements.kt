package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.Assignability
import com.github.derg.transpiler.source.Id
import com.github.derg.transpiler.source.Name
import com.github.derg.transpiler.source.Passability
import com.github.derg.transpiler.source.ast.*
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.hir.Function
import com.github.derg.transpiler.util.*

/**
 * Defines the function based on the contends provided by the [definition]. Once defined, the function will be ready
 * for usage in all other use-cases, such as optimization and code generation.
 */
private fun Function.define(definition: Definition.Function): Result<Unit, ResolveError>
{
    val converter = ConverterStatements(symbols)
    converter.prepare(definition.statements).onFailure { return failureOf(it) }
    instructions = converter.convert(definition.statements).valueOr { return failureOf(it) }
    return successOf()
}

/**
 *
 */
class ConverterStatements(private val symbols: SymbolTable)
{
    /**
     * Performs a pre-pass on all provided [statements].
     *
     * Certain statements may involve the structure of the program and thus does not count as executable code. Such
     * statements will not be translated into any instruction, but they will contribute to the symbol table. This allows
     * functions and types to be named and retrieved at any point within the instruction list.
     */
    fun prepare(statements: List<Statement>): Result<Unit, ResolveError>
    {
        // Must ensure all functions and types are declared in advance, as they may be referring to each other in a
        // non-trivial manners. At any scope level, all functions and types must be able to "see" each other, even
        // before the later functions and types have been declared in source code.
        val types = statements
            .filterIsInstance<Definition.Type>()
            .map { it to it.toDeclaration().valueOr { err -> return failureOf(err) } }
            .onEach { symbols.register(it.second) }
        val functions = statements
            .filterIsInstance<Definition.Function>()
            .map { it to it.toDeclaration().valueOr { err -> return failureOf(err) } }
            .onEach { symbols.register(it.second) }
        
        // Variables may depend on each other, so the order in which they are defined matters. They must be defined
        // before any functions are defined, as the functions may depend on those variables.
        for (variable in statements.filterIsInstance<Definition.Variable>())
            variable.toDeclaration().valueOr { return failureOf(it) }.also { symbols.register(it) }
        
        // All required symbols are now valid, can register them in the symbol table, they may now be defined too.
        // TODO: Define types, too
        functions.fold { it.second.define(it.first) }.onFailure { return failureOf(it) }
        
        return successOf()
    }
    
    /**
     * Converts the list of [statements] into a sequence of specific instructions which are to be performed in order.
     */
    fun convert(statements: List<Statement>): Result<List<Instruction>, ResolveError> =
        statements.fold { convert(it) }
    
    fun convert(statement: Statement): Result<Instruction, ResolveError> = when (statement)
    {
        is Assignment.Assign   -> statement.toInstruction()
        is Control.Branch      -> statement.toInstruction()
        is Control.Invoke      -> TODO()
        is Control.Raise       -> statement.toInstruction()
        is Control.Return      -> statement.toInstruction()
        is Definition.Function -> Nop.toSuccess()
        is Definition.Type     -> Nop.toSuccess()
        is Definition.Variable -> statement.toInstruction()
    }
    
    private fun Assignment.Assign.toInstruction(): Result<Instruction, ResolveError>
    {
        val variable = symbols.find(name).firstOrNull() ?: return failureOf(ResolveError.Unknown)
        if (variable !is Variable)
            return ResolveError.Unknown.toFailure()
        
        val value = resolveValue(expression).valueOr { return failureOf(it) }
        if (variable.type != value.type)
            return ResolveError.Unknown.toFailure()
        
        return Assign(variable, value).toSuccess()
    }
    
    private fun Control.Branch.toInstruction(): Result<Instruction, ResolveError>
    {
        val value = resolveValue(predicate).valueOr { return failureOf(it) }
        if (value !is ValueBool)
            return ResolveError.Unknown.toFailure()
        
        val sSymbols = SymbolTable(symbols)
        val fSymbols = SymbolTable(symbols)
        val sConv = ConverterStatements(sSymbols)
        val fConv = ConverterStatements(fSymbols)
        
        return Condition(
            predicate = value,
            success = Condition.Branch(sSymbols, sConv.convert(success).valueOr { return failureOf(it) }),
            failure = Condition.Branch(fSymbols, fConv.convert(failure).valueOr { return failureOf(it) }),
        ).toSuccess()
    }
    
    private fun Control.Raise.toInstruction(): Result<Instruction, ResolveError> =
        resolveValue(expression).mapValue { Raise(it) }
    
    private fun Control.Return.toInstruction(): Result<Instruction, ResolveError> =
        if (expression == null) Exit.toSuccess() else resolveValue(expression).mapValue { Return(it) }
    
    private fun Definition.Variable.toInstruction(): Result<Instruction, ResolveError>
    {
        val variable = symbols.find(name).firstOrNull() ?: return failureOf(ResolveError.Unknown)
        if (variable !is Variable)
            return ResolveError.Unknown.toFailure()
        
        val value = resolveValue(value).valueOr { return failureOf(it) }
        if (variable.type != value.type)
            return ResolveError.Unknown.toFailure()
        
        return Assign(variable, value).toSuccess()
    }
    
    // ...
    
    private fun Definition.Type.toDeclaration(): Result<Type, ResolveError>
    {
        TODO()
    }
    
    private fun Definition.Function.toDeclaration(): Result<Function, ResolveError>
    {
        return Function(
            id = Id.randomUUID(),
            name = name,
            visibility = visibility,
            value = resolveTypeId(valueType).valueOr { return failureOf(it) } ?: Builtin.VOID.id,
            error = resolveTypeId(errorType).valueOr { return failureOf(it) } ?: Builtin.VOID.id,
            params = parameters.fold { it.toDeclaration() }.valueOr { return failureOf(it) },
            symbols = SymbolTable(symbols),
        ).toSuccess()
    }
    
    private fun Parameter.toDeclaration(): Result<Function.Parameter, ResolveError>
    {
        val type = resolveTypeId(type).valueOr { return failureOf(it) }
        val value = if (value == null) null else resolveValue(value).valueOr { return failureOf(it) }
        val combined = type ?: value?.type ?: return ResolveError.Unknown.toFailure()
        
        return Function.Parameter(
            type = combined,
            name = name,
            passability = Passability.IN,
            value = value,
        ).toSuccess()
    }
    
    private fun Definition.Variable.toDeclaration(): Result<Variable, ResolveError>
    {
        // Note: Type inference fails here if the order in which variables are declared does not correspond to the
        //       order in which they are initialized.
        val type = resolveTypeId(type).valueOr { return failureOf(it) }
            ?: resolveValue(value).valueOr { return failureOf(it) }.type
        
        return Variable(
            id = Id.randomUUID(),
            name = name,
            visibility = visibility,
            mutability = mutability,
            assignability = Assignability.ASSIGNABLE,
            type = type,
        ).toSuccess()
    }
    
    /**
     * Retrieves the id of the [Type] with the given [name] in the current scope. If the name is not provided, no type
     * is expected and [Builtin.VOID] is returned instead.
     */
    private fun resolveTypeId(name: Name?): Result<Id?, ResolveError>
    {
        if (name == null)
            return successOf(null)
        
        val type = symbols.find(name).filterIsInstance<Type>().firstOrNull()
        return type?.id?.toSuccess() ?: ResolveError.Unknown.toFailure()
    }
    
    /**
     * Converts the [expression] into a value, if it is provided. If no expression was provided, no value is returned.
     */
    private fun resolveValue(expression: Expression): Result<Value, ResolveError> =
        ConverterExpressions(symbols).convert(expression)
}
