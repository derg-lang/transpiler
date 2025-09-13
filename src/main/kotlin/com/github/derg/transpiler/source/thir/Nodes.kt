package com.github.derg.transpiler.source.thir

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
    val valueType: ThirType
    val errorType: ThirType
    
    /**
     * Boolean values, `true` and `false`.
     */
    data class Bool(val raw: Boolean) : ThirExpression
    {
        override val valueType: ThirType get() = ThirType.Bool
        override val errorType: ThirType get() = ThirType.Void
    }
    
    /**
     * 32-bit signed integers.
     */
    data class Int32(val raw: Int) : ThirExpression
    {
        override val valueType: ThirType get() = ThirType.Int32
        override val errorType: ThirType get() = ThirType.Void
    }
    
    /**
     * 64-bit signed integers.
     */
    data class Int64(val raw: Long) : ThirExpression
    {
        override val valueType: ThirType get() = ThirType.Int64
        override val errorType: ThirType get() = ThirType.Void
    }
    
    /**
     * 32-bit floating point numbers.
     */
    data class Float32(val raw: Float) : ThirExpression
    {
        override val valueType: ThirType get() = ThirType.Float32
        override val errorType: ThirType get() = ThirType.Void
    }
    
    /**
     * 64-bit floating point numbers.
     */
    data class Float64(val raw: Double) : ThirExpression
    {
        override val valueType: ThirType get() = ThirType.Float64
        override val errorType: ThirType get() = ThirType.Void
    }
    
    /**
     * Unicode character sequences.
     */
    data class Str(val raw: String) : ThirExpression
    {
        override val valueType: ThirType get() = ThirType.Str
        override val errorType: ThirType get() = ThirType.Void
    }
    
    /**
     * Data type.
     */
    data class Type(val raw: ThirType) : ThirExpression
    {
        override val valueType: ThirType get() = ThirType.Type
        override val errorType: ThirType get() = ThirType.Void
    }
    
    /**
     * Retrieves a symbol value from memory.
     */
    data class Load(val symbolId: UUID, override val valueType: ThirType) : ThirExpression
    {
        override val errorType: ThirType get() = ThirType.Void
    }
    
    /**
     * Invokes the callable [instance] using the provided [parameters].
     */
    data class Call(
        val instance: ThirExpression,
        val parameters: List<ThirExpression>,
        override val valueType: ThirType,
        override val errorType: ThirType,
    ) : ThirExpression
    
    /**
     * Performs a catch on the [lhs] expression, replacing the failure value in the [lhs] with the success value of the
     * [rhs] expression handling the failure. Otherwise, the [rhs] expression is returner as a success or a failure
     * expression.
     */
    data class Catch(val lhs: ThirExpression, val rhs: ThirExpression, val operator: CatchOperator) : ThirExpression
    {
        override val valueType: ThirType get() = lhs.valueType
        override val errorType: ThirType get() = ThirType.Void
    }
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
    val type: ThirType
    
    /**
     * Constants represents bindings evaluated at compile-time.
     */
    data class Const(
        override val id: UUID,
        override val name: String,
        override val type: ThirType,
        var def: ConstDef?,
    ) : ThirDeclaration
    
    data class ConstDef(
        val value: ThirExpression,
    )
    
    /**
     * Fields represents a value or attribute associated with an instance of a type.
     */
    data class Field(
        override val id: UUID,
        override val name: String,
        override val type: ThirType,
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
        val valueType: ThirType,
        val errorType: ThirType,
        val genericTypeIds: List<UUID>,
        val genericValueIds: List<UUID>,
        val parameterIds: List<UUID>,
        var def: FunctionDef?,
    ) : ThirDeclaration
    {
        override val type: ThirType get() = ThirType.Function(valueType, errorType)
    }
    
    data class FunctionDef(
        val statements: List<ThirStatement>,
    )
    
    /**
     * Parameters are values which are passed into functions.
     */
    data class Parameter(
        override val id: UUID,
        override val name: String,
        val passability: Passability,
        override val type: ThirType,
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
        val genericTypeIds: List<UUID>,
        val genericValueIds: List<UUID>,
        val fieldIds: List<UUID>,
        var def: StructureDef?,
    ) : ThirDeclaration
    {
        override val type: ThirType get() = ThirType.Type
    }
    
    data class StructureDef(
        val placeholder: Nothing?,
    )
    
    /**
     * Variables represents bindings evaluated at run-time.
     */
    data class Variable(
        override val id: UUID,
        override val name: String,
        override val type: ThirType,
        var def: VariableDef?,
    ) : ThirDeclaration
    
    data class VariableDef(
        val value: ThirExpression,
    )
    
    /**
     * Generic type parameter, represents a type known at or computed during compile-time execution.
     */
    data class GenericType(
        override val id: UUID,
        override val name: String,
        val conceptIds: List<UUID>,
        var def: GenericTypeDef?,
    ) : ThirDeclaration
    {
        override val type: ThirType get() = ThirType.Type
    }
    
    data class GenericTypeDef(
        val default: ThirType?,
    )
    
    /**
     * Generic expression parameter, represents a value known at or computed during compile-time execution.
     */
    data class GenericValue(
        override val id: UUID,
        override val name: String,
        override val type: ThirType,
        var def: GenericValueDef?,
    ) : ThirDeclaration
    
    data class GenericValueDef(
        val default: ThirExpression?,
    )
}

// Here is some code samples involving simple type checking:
/*
val a = b
val b = 0
*/
val b = ThirDeclaration.Const(
    id = UUID.randomUUID(),
    name = "b",
    type = ThirType.Int32,
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Int32(0),
    )
)
val a = ThirDeclaration.Const(
    id = UUID.randomUUID(),
    name = "a",
    type = ThirType.Int32,
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Load(b.id, b.type)
    )
)

// Here is some code samples involving generics:
/*
struct Ptr[Type]
{
    // builtin
}

struct Array[Element, size: Int32]
{
    val data: Ptr[Type = Element]
    val test = size
}

val collection: Array[Bool, 17] = magic()
val foo = 42
val bar: Array[Str, foo] = magic()
*/
val ptrType = ThirDeclaration.GenericType(
    id = UUID.randomUUID(),
    name = "Type",
    conceptIds = emptyList(),
    def = ThirDeclaration.GenericTypeDef(
        default = null,
    )
)
val ptr = ThirDeclaration.Structure(
    id = UUID.randomUUID(),
    name = "Ptr",
    genericTypeIds = listOf(ptrType.id),
    genericValueIds = emptyList(),
    fieldIds = emptyList(),
    def = ThirDeclaration.StructureDef(null),
)

val arrayElement = ThirDeclaration.GenericType(
    id = UUID.randomUUID(),
    name = "Element",
    conceptIds = emptyList(),
    def = ThirDeclaration.GenericTypeDef(
        default = null,
    )
)
val arraySize = ThirDeclaration.GenericValue(
    id = UUID.randomUUID(),
    name = "size",
    type = ThirType.Int32,
    def = ThirDeclaration.GenericValueDef(
        default = null,
    )
)
val arrayData = ThirDeclaration.Field(
    id = UUID.randomUUID(),
    name = "data",
    type = ThirType.Instance(ptr.id, mapOf(ptrType.id to ThirType.Structure(arrayElement.id)), emptyMap()),
    def = ThirDeclaration.FieldDef(
        default = null,
    )
)
val arrayTest = ThirDeclaration.Field(
    id = UUID.randomUUID(),
    name = "test",
    type = ThirType.Int32,
    def = ThirDeclaration.FieldDef(
        default = ThirExpression.Load(arraySize.id, arraySize.type),
    )
)
val array = ThirDeclaration.Structure(
    id = UUID.randomUUID(),
    name = "Array",
    genericTypeIds = listOf(arrayElement.id),
    genericValueIds = listOf(arraySize.id),
    fieldIds = listOf(arrayData.id, arrayTest.id),
    def = ThirDeclaration.StructureDef(null),
)

val collection = ThirDeclaration.Const(
    id = UUID.randomUUID(),
    name = "collection",
    type = ThirType.Instance(array.id, mapOf(arrayElement.id to ThirType.Bool), mapOf(arraySize.id to ThirExpression.Int32(17))),
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Load(UUID.randomUUID(), ThirType.Structure(array.id)),
    )
)
val foo = ThirDeclaration.Const(
    id = UUID.randomUUID(),
    name = "foo",
    type = ThirType.Int32,
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Int32(42),
    )
)
val bar = ThirDeclaration.Const(
    id = UUID.randomUUID(),
    name = "bar",
    type = ThirType.Instance(array.id, mapOf(arrayElement.id to ThirType.Str), mapOf(arraySize.id to ThirExpression.Load(foo.id, foo.type))),
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Load(UUID.randomUUID(), ThirType.Structure(array.id)),
    )
)

// Here is some code samples involving concepts:
/*
concept Collection[Type]
{
    fun iterator() -> Iterator[Type]
}

concept Iterator[Type]
{
    fun next(): None -> Type
}

struct CustomArray[Element = Bool, capacity: Int = 42]
{
    var size: Int
    val data: Ptr[Type]
    
    export fun thingy()
    {
    }

    private fun whatever()
    {
    }

Collection[Type = Element]:
    fun iterator(): CustomIterator[Element]
    {
        return CustomIterator(index = 0, size = size, data = data)
    }

Collection[Type = Bool]:
    fun iterator(): SpecialIterator
    {
        return SpecialIterator(index = 0, size = size, data = data)
    }
}

struct CustomIterator[Element]
{
    var index: Int
    val size: Int
    ref data: Ptr[Element]

Iterator[Type = Element]:
    fun next(): None -> Element
    {
        if index >= size
            raise None
        val current = index
        index += 1
        return data(current)
    }
}

struct Data[Type]
{
    val id: Int
    val name: Str
    val value: Type
}
*/
