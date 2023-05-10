package com.github.derg.transpiler.phases.resolver

import com.github.derg.transpiler.source.ast.Assignment
import com.github.derg.transpiler.source.ast.Control
import com.github.derg.transpiler.source.ast.Definition
import com.github.derg.transpiler.source.ast.Statement
import com.github.derg.transpiler.source.hir.*
import com.github.derg.transpiler.source.hir.Function
import com.github.derg.transpiler.util.*

/**
 * Defines the function based on the contents provided by the [definition]. Once defined, the function will be ready
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
 * Defines the type based on the contents provided by the [definition].
 */
private fun Type.define(definition: Definition.Type): Result<Unit, ResolveError>
{
    // TODO: Implement me
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
            .fold { t -> DeclaratorType(symbols)(t).mapValue { t to it } }
            .valueOr { return failureOf(it) }
        val functions = statements
            .filterIsInstance<Definition.Function>()
            .fold { f -> DeclaratorFunction(symbols)(f).mapValue { f to it } }
            .valueOr { return failureOf(it) }
        
        // Variables may depend on each other, so the order in which they are defined matters. They must be declared
        // before any functions are defined, as the functions may depend on those variables.
        statements
            .filterIsInstance<Definition.Variable>()
            .fold { DeclaratorVariable(symbols)(it) }
            .onFailure { return failureOf(it) }
        
        // All required symbols are now valid, can register them in the symbol table, they may now be defined too.
        functions.fold { it.second.define(it.first) }.onFailure { return failureOf(it) }
        types.fold { it.second.define(it.first) }.onFailure { return failureOf(it) }
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
        
        val value = symbols.resolveRequiredValue(expression).valueOr { return failureOf(it) }
        if (variable.type != value.type)
            return ResolveError.Unknown.toFailure()
        
        return Assign(variable, value).toSuccess()
    }
    
    private fun Control.Branch.toInstruction(): Result<Instruction, ResolveError>
    {
        val value = symbols.resolveRequiredValue(predicate).valueOr { return failureOf(it) }
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
        symbols.resolveRequiredValue(expression).mapValue { Raise(it) }
    
    private fun Control.Return.toInstruction(): Result<Instruction, ResolveError> =
        if (expression == null) Exit.toSuccess() else symbols.resolveRequiredValue(expression).mapValue { Return(it) }
    
    private fun Definition.Variable.toInstruction(): Result<Instruction, ResolveError>
    {
        val variable = symbols.find(name).firstOrNull() ?: return failureOf(ResolveError.Unknown)
        if (variable !is Variable)
            return ResolveError.Unknown.toFailure()
        
        val value = symbols.resolveRequiredValue(value).valueOr { return failureOf(it) }
        if (variable.type != value.type)
            return ResolveError.Unknown.toFailure()
        
        return Assign(variable, value).toSuccess()
    }
}
