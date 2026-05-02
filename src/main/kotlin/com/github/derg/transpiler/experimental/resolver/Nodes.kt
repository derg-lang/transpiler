package com.github.derg.transpiler.experimental.resolver

import com.github.derg.transpiler.source.*
import java.util.*

/**
 * Expressions are computable bits of code which resolves down to a value. These code elements cannot be re-assigned to
 * other values, and do not occupy any space in memory. The final value will either be used as a parameter for a
 * procedure call, or stored in a variable, at which point some memory is necessary to hold it.
 *
 * Note that expressions may be used as types as well, due to types being first-class citizens. This means that a type
 * may be passed around as parameters, returned from functions, and generally used during compile-time. This allows the
 * developers to write programs which manipulates types.
 */
sealed interface ThirExpression
{
    /**
     * Canonical expressions are reduced to their simplest forms. These are the forms of expressions which are folded
     * into their simplest representations, or have been evaluated to something which is in its canonical form.
     */
    sealed interface Canonical : ThirExpression
    
    /**
     * A location in memory.
     */
    data class Address(val raw: Int) : Canonical
    
    /**
     * Boolean values, `true` and `false`.
     */
    data class Bool(val raw: Boolean) : Canonical
    
    /**
     * 32-bit signed integers.
     */
    data class Int32(val raw: Int) : Canonical
    
    /**
     * 64-bit signed integers.
     */
    data class Int64(val raw: Long) : Canonical
    
    /**
     * 32-bit floating point numbers.
     */
    data class Float32(val raw: Float) : Canonical
    
    /**
     * 64-bit floating point numbers.
     */
    data class Float64(val raw: Double) : Canonical
    
    /**
     * Unicode character sequences.
     */
    data class Str(val raw: String) : Canonical
    
    /**
     * Data type as a value, used for compile-time computation with type information.
     */
    data class Type(val raw: ThirType) : Canonical
    
    /**
     * An instance containing any number of [fields].
     */
    data class Instance(val fields: MutableMap<String, Canonical>) : Canonical
    
    /**
     * Retrieves the value of the given [name] from the given [instance].
     */
    data class Field(val instance: ThirExpression, val name: String) : ThirExpression
    
    /**
     * Invokes a callable using the provided [parameters].
     */
    data class Call(val instance: ThirExpression, val parameters: List<ThirExpression>) : ThirExpression
    
    /**
     * Performs a catch on the [lhs] expression, replacing the failure value in the [lhs] with the success value of the
     * [rhs] expression handling the failure. Otherwise, the [rhs] expression is returner as a success or a failure
     * expression.
     */
    data class Catch(val lhs: ThirExpression, val rhs: ThirExpression, val operator: CatchOperator) : ThirExpression
}

/**
 * Statements are typically considered the executable parts of the program. They direct the control flow, handles side
 * effects, determines how the expressions are used after evaluation, and so on.
 */
sealed interface ThirStatement
{
    /**
     * Assigns the specified [expression] to the object located under the given [instance]. Note that only a single form
     * of assignment exists in this layer; other assignment operators are de-sugared into function calls combined with
     * this one assignment approach.
     */
    data class Assign(val instance: ThirExpression, val expression: ThirExpression) : ThirStatement
    
    /**
     * Evaluates the [expression], and executes any side effects which may arise as a consequence. The [expression] is
     * not permitted to evaluate to any non-void value or error.
     */
    data class Evaluate(val expression: ThirExpression) : ThirStatement
    
    /**
     * Conditional execution is possible by branching the control flow one a [predicate]. If the predicates matches, the
     * [success] branch is selected, otherwise the [failure] branch is selected.
     */
    data class If(val predicate: ThirExpression, val success: List<ThirStatement>, val failure: List<ThirStatement>) : ThirStatement
    
    /**
     * Specifies that the execution flow should exit the current function, returning control flow back to the caller.
     * Note that the function cannot be exited in this manner if the function expects a return value.
     */
    data object Return : ThirStatement
    
    /**
     * Specifies that a function should raise a specific [expression]. The raise statement functions similarly to the
     * return statement, in that execution is resumed to the caller of the function. When a value is raised from a
     * function, this usually indicates that the function has failed to uphold its contract and produced a value which
     * does not conform to the expectations of the caller.
     */
    data class ReturnError(val expression: ThirExpression) : ThirStatement
    
    /**
     * Specifies that a function should return a specific [expression]. The return statement marks the point where the
     * execution is resumed to the caller of the function, executing nothing else in the body of the function.
     * Returning a value from a function indicates usually that the function has succeeded in producing a usable value.
     */
    data class ReturnValue(val expression: ThirExpression) : ThirStatement
    
    /**
     * While loops are a control flow construct which execute the [statements] until the [predicate] evaluates to false.
     */
    data class While(val predicate: ThirExpression, val statements: List<ThirStatement>) : ThirStatement
}

/**
 * Declarations are structural components of the source code, such as functions, data structures, unions, and so on.
 */
sealed interface ThirDeclaration
{
    val id: UUID
    val name: String
    
    /**
     * Constants represents bindings evaluated at compile-time.
     */
    data class Constant(
        override val id: UUID,
        override val name: String,
        val kind: ThirKind,
        var def: ConstantDef?,
    ) : ThirDeclaration
    
    data class ConstantDef(
        val value: ThirExpression.Canonical,
    )
    
    /**
     * Fields represents a value or attribute associated with an instance of a type.
     */
    data class Field(
        override val id: UUID,
        override val name: String,
        val assignability: Assignability,
        val kind: ThirKind,
        var def: FieldDef?,
    ) : ThirDeclaration
    
    data class FieldDef(
        val default: ThirExpression?,
    )
    
    /**
     * Functions represents a subroutine which may be invoked to produce a single value.
     */
    data class Function(
        override val id: UUID,
        override val name: String,
        val valueKind: ThirKind,
        val errorKind: ThirKind,
        val typeParameterIds: List<UUID>,
        val parameterIds: List<UUID>,
        var def: FunctionDef?,
    ) : ThirDeclaration
    
    data class FunctionDef(
        val statements: List<ThirStatement>,
    )
    
    /**
     * Methods represents a subroutine which may be invoked to produce a single value.
     */
    data class Method(
        override val id: UUID,
        override val name: String,
        val mutability: Mutability,
        val valueKind: ThirKind,
        val errorKind: ThirKind,
        val typeParameterIds: List<UUID>,
        val parameterIds: List<UUID>,
        var def: MethodDef?,
    ) : ThirDeclaration
    
    data class MethodDef(
        val statements: List<ThirStatement>,
    )
    
    /**
     * Parameters are values which are passed into functions.
     */
    data class Parameter(
        override val id: UUID,
        override val name: String,
        val passability: Passability,
        val kind: ThirKind,
        var def: ParameterDef?,
    ) : ThirDeclaration
    
    data class ParameterDef(
        val default: ThirExpression?,
    )
    
    /**
     * Structures represent data laid out in memory in a particular manner.
     */
    data class Structure(
        override val id: UUID,
        override val name: String,
        val typeParameterIds: List<UUID>,
        val ctorEntryIds: List<UUID>, // The collection of ONLY constructor parameters.
        val fieldIds: List<UUID>, // The collection of ALL fields, including constructor parameters.
        val methodIds: List<UUID>,
        var def: StructureDef?,
    ) : ThirDeclaration
    
    data class StructureDef(
        val placeholder: Nothing?,
    )
    
    /**
     * Parameters are values which are passed into functions.
     */
    data class TypeParameter(
        override val id: UUID,
        override val name: String,
        val kind: ThirKind,
        var def: TypeParameterDef?,
    ) : ThirDeclaration
    
    data class TypeParameterDef(
        val default: ThirExpression.Canonical?,
    )
    
    /**
     * Variables represents bindings evaluated at run-time.
     */
    data class Variable(
        override val id: UUID,
        override val name: String,
        val assignability: Assignability,
        val kind: ThirKind,
        var def: VariableDef?,
    ) : ThirDeclaration
    
    data class VariableDef(
        val value: ThirExpression,
    )
}
