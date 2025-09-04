package com.github.derg.transpiler.source.hir

import com.github.derg.transpiler.source.*
import com.github.derg.transpiler.utils.*
import java.math.*
import java.util.*

/**
 * Every element within the abstract syntax tree may be represented as a node. Each node is merely a piece of the
 * program, ranging from individual tokens like constants, to large sequences of tokens such as a whole data structure.
 */
sealed interface HirNode
{
    /**
     * The unique identifier for this specific node.
     */
    val id: UUID
}

/**
 * Expressions are computable bits of code which resolves down to a value. These code elements cannot be re-assigned to
 * other values, and do not occupy any space in memory. The final value will either be used as a parameter for a
 * procedure call, or stored in a variable, at which point some memory is necessary to hold it.
 *
 * Note that expressions may be used as types as well, due to types being first-class citizens. This means that a type
 * may be passed around as parameters, returned from functions, and generally used during compile-time. This allows the
 * developers to write programs which manipulates types.
 */
sealed interface HirExpression : HirNode
{
    /**
     * Loads the value bound to the identifier with the given [name]. The identifier may refer to either a value bound
     * to any memory address, or any named symbol. When referring to a function, [typeParameters] may be used to
     * disambiguate which instance is intended.
     *
     * Loads can be used to retrieve the value of any named node, be it variable, function, structure, union, or any
     * other type.
     */
    data class Identifier(override val id: UUID, val name: String, val typeParameters: List<NamedMaybe<HirExpression>>) : HirExpression
    
    /**
     * Invokes the callable [instance] using the provided [parameters]. The arguments are specified in the same order
     * in which they appear in source code. In some cases, the callable cannot be resolved without additional
     * information about the callable template specialization.
     */
    data class Call(override val id: UUID, val instance: HirExpression, val parameters: List<NamedMaybe<HirExpression>>) : HirExpression
    
    /**
     * Retrieves the value of the given [identifier] from the given [instance]. The object may be an object which exists
     * on the stack or heap.
     */
    data class Field(override val id: UUID, val instance: HirExpression, val identifier: Identifier) : HirExpression
    
    /**
     * Booleans are either `true` or `false`, specified in [value].
     */
    data class Bool(override val id: UUID, val value: Boolean) : HirExpression
    
    /**
     * Integer types of an arbitrary size, specified in [value].
     */
    data class Integer(override val id: UUID, val value: BigInteger, val literal: String) : HirExpression
    
    /**
     * Decimal types of an arbitrary size and precision, specified in [value].
     */
    data class Decimal(override val id: UUID, val value: BigDecimal, val literal: String) : HirExpression
    
    /**
     * Strings of an arbitrary length, specified in [value].
     */
    data class Text(override val id: UUID, val value: String, val literal: String) : HirExpression
    
    // TODO: Add in an if-expression as well. It is different from the if-statement, in the sense that is always return
    //       either the success of failure branch value. It does have a return value, and this return value cannot be
    //       unit type.
}

/**
 * Statements are typically considered the executable parts of the program. They direct the control flow, handles side
 * effects, determines how the expressions are used after evaluation, and so on.
 */
sealed interface HirStatement
{
    /**
     * Assigns the specified [expression] to the object located under the given [instance]. The expression is assigned
     * to the instance using the given [operator].
     */
    data class Assign(val instance: HirExpression, val expression: HirExpression, val operator: AssignOperator) : HirStatement
    
    /**
     * A body represents a sequence of any number of statements. They can be used to describe a certain scope,
     * indicating a level of visibility or lifetime for bindings.
     */
    // data class Block(val statements: List<HirStatement>) : HirStatement
    
    /**
     * Evaluates the [expression], and executes any side effects which may arise as a consequence. The [expression] is
     * not permitted to evaluate to any non-void value or error.
     */
    data class Evaluate(val expression: HirExpression) : HirStatement
    
    /**
     * Loops are a control flow construct which iterates over the [expression] until all elements in the expression has
     * been looped over. The [identifier] denotes the name of the variable which holds the current element in the loop.
     * The [body] block will be repeatedly executed until no more elements remains in the loop.
     */
    data class For(val identifier: String, val expression: HirExpression, val body: List<HirStatement>) : HirStatement
    
    /**
     * Conditional execution is possible by branching the control flow one a [predicate]. If the predicates matches, the
     * [success] branch is selected, otherwise the [failure] branch is selected.
     */
    data class If(val predicate: HirExpression, val success: List<HirStatement>, val failure: List<HirStatement>) : HirStatement
    
    /**
     * Specifies that the execution flow should exit the current function, returning control flow back to the caller.
     * Note that the function cannot be exited in this manner if the function expects a return value.
     */
    data object Return : HirStatement
    
    /**
     * Specifies that a function should raise a specific [expression]. The raise statement functions similarly to the
     * return statement, in that execution is resumed to the caller of the function. When a value is raised from a
     * function, this usually indicates that the function has failed to uphold its contract and produced a value which
     * does not conform to the expectations of the caller.
     */
    data class ReturnError(val expression: HirExpression) : HirStatement
    
    /**
     * Specifies that a function should return a specific [expression]. The return statement marks the point where the
     * execution is resumed to the caller of the function, executing nothing else in the body of the function.
     * Returning a value from a function indicates usually that the function has succeeded in producing a usable value.
     */
    data class ReturnValue(val expression: HirExpression) : HirStatement
    
    /**
     * While loops are a control flow construct which execute the [body] until the [predicate] evaluates to false.
     */
    data class While(val predicate: HirExpression, val body: List<HirStatement>) : HirStatement
    
    /**
     * Variables are units which hold a specific [value] and associates the value with a binding [name]. Variables may
     * optionally be given a [type], which is verified against the actual type of the expression. If the [type] is not
     * specified, it is inferred from [value]. Depending on the [assignability] of the variable, it may either be
     * assigned a new value or not.
     */
    data class Variable(
        val id: UUID,
        val name: String,
        val type: HirType?,
        val value: HirExpression,
        val assignability: Assignability,
    ) : HirStatement
}

/**
 * Declarations are structural components of the source code, such as functions, data structures, unions, and so on.
 */
sealed interface HirDeclaration : HirNode
{
    /**
     * Constants are units which hold a specific [value] value and associates the value with a binding [name].
     * Constants may optionally be given a [type], which is verified against the actual type of the expression. If the
     * [type] is not specified, it is inferred from its [value].
     */
    data class ConstantDecl(
        override val id: UUID,
        val name: String,
        val type: HirType?,
        val value: HirExpression,
    ) : HirDeclaration
    
    /**
     * Fields represents a value or attribute associated with an instance of a type.
     */
    data class FieldDecl(
        override val id: UUID,
        val name: String,
        val type: HirType?,
        val default: HirExpression?,
        val visibility: Visibility,
        val assignability: Assignability,
    ) : HirNode
    
    /**
     * Functions are callable subroutines which allows a program to be structured into smaller segments.
     */
    data class FunctionDecl(
        override val id: UUID,
        val name: String,
        val visibility: Visibility,
        val typeParameters: List<TypeParameterDecl>,
        val parameters: List<ParameterDecl>,
        val valueType: HirType?,
        val errorType: HirType?,
        val body: List<HirStatement>,
    ) : HirDeclaration
    
    /**
     * The parameter represents a specific input expected to a callable construct.
     */
    data class ParameterDecl(
        override val id: UUID,
        val name: String,
        val type: HirType,
        val default: HirExpression?,
        val passability: Passability,
    ) : HirNode
    
    /**
     * A segment represents a single source file of code.
     */
    data class SegmentDecl(
        override val id: UUID,
        val imports: List<String>,
        val constants: List<ConstantDecl>,
        val functions: List<FunctionDecl>,
        val structures: List<StructureDecl>,
    ) : HirNode
    
    /**
     * Data structures represents a collection of properties. It does not provide any additional functionality.
     */
    data class StructureDecl(
        override val id: UUID,
        val name: String,
        val typeParameters: List<TypeParameterDecl>,
        val fields: List<FieldDecl>,
        val visibility: Visibility,
    ) : HirDeclaration
    
    /**
     * Type parameters enable a developer to specify type information during compile time. These parameters are used to
     * refine which generics are used in expressions.
     */
    data class TypeParameterDecl(
        override val id: UUID,
        val name: String,
        val type: HirType?,
        val default: HirExpression?,
    ) : HirNode
}

/**
 * The base type representation for the type system.
 */
sealed interface HirType
{
    /**
     * Expressions represents a type which has not yet been computed, or some constant term which should be used as a
     * compile-time value.
     *
     * @param value The value which the type should be considered to be.
     */
    data class Expression(val value: HirExpression) : HirType
    
    /**
     * Function types, which describes a callable object. Functions are permitted to return either a value type, or an
     * error type, never both. Functions are allowed to take an arbitrary number of parameters.
     *
     * @param valueType The value type returned by this callable.
     * @param errorType The error type returned by this callable.
     * @param parameters The parameters which are acceptable by this callable.
     */
    data class Function(val valueType: HirType?, val errorType: HirType?, val parameters: List<Parameter>) : HirType
    
    /**
     * Parameters represents an input into a callable object.
     *
     * @param name The name of the parameter.
     * @param type The type information of the value provided with this parameter.
     * @param default The default value of the parameter, if any.
     * @param passability The specific mechanism of how the parameter should be passed into the function.
     */
    data class Parameter(val name: String, val type: HirType, val default: HirExpression?, val passability: Passability)
    
    /**
     * Type types, which describe an arbitrary type. These things represent type information, rather than a value
     * adhering to a specific type.
     */
    data object Type : HirType
}
