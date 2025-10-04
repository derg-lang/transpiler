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
    /**
     * Canonical expressions are reduced to their simplest forms. These are the forms of expressions which are folded
     * into their simplest representations, or have been evaluated to something which is in its canonical form.
     */
    sealed interface Canonical : ThirExpression
    
    val valueKind: ThirKind
    val errorKind: ThirKind
    
    /**
     * Boolean values, `true` and `false`.
     */
    data class Bool(val raw: Boolean) : Canonical
    {
        override val valueKind: ThirKind get() = ThirKind.Value(ThirType.Bool)
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * 32-bit signed integers.
     */
    data class Int32(val raw: Int) : Canonical
    {
        override val valueKind: ThirKind get() = ThirKind.Value(ThirType.Int32)
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * 64-bit signed integers.
     */
    data class Int64(val raw: Long) : Canonical
    {
        override val valueKind: ThirKind get() = ThirKind.Value(ThirType.Int64)
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * 32-bit floating point numbers.
     */
    data class Float32(val raw: Float) : Canonical
    {
        override val valueKind: ThirKind get() = ThirKind.Value(ThirType.Float32)
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * 64-bit floating point numbers.
     */
    data class Float64(val raw: Double) : Canonical
    {
        override val valueKind: ThirKind get() = ThirKind.Value(ThirType.Float64)
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * Unicode character sequences.
     */
    data class Str(val raw: String) : Canonical
    {
        override val valueKind: ThirKind get() = ThirKind.Value(ThirType.Str)
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * Data type as a value, used for compile-time computation with type information.
     */
    data class Type(val raw: ThirType) : Canonical
    {
        override val valueKind: ThirKind get() = ThirKind.Type
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * Instance of a structure.
     *
     * @param fields The current values every field of the structure has in this instance.
     */
    data class Instance(val fields: MutableMap<UUID, Canonical>, override val valueKind: ThirKind) : Canonical
    {
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * Invokes the callable [instance] using the provided [parameters].
     */
    data class Call(
        val instance: ThirExpression,
        val parameters: List<ThirExpression>,
        override val valueKind: ThirKind,
        override val errorKind: ThirKind,
    ) : ThirExpression
    
    /**
     * Performs a catch on the [lhs] expression, replacing the failure value in the [lhs] with the success value of the
     * [rhs] expression handling the failure. Otherwise, the [rhs] expression is returner as a success or a failure
     * expression.
     */
    data class Catch(val lhs: ThirExpression, val rhs: ThirExpression, val operator: CatchOperator) : ThirExpression
    {
        override val valueKind: ThirKind get() = lhs.valueKind
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * Retrieves the value of the given [fieldId] from the given [instance].
     */
    data class Field(val instance: ThirExpression, val fieldId: UUID, override val valueKind: ThirKind) : ThirExpression
    {
        override val errorKind: ThirKind get() = ThirKind.Nothing
    }
    
    /**
     * Retrieves a symbol value from memory.
     */
    data class Load(val symbolId: UUID, override val valueKind: ThirKind) : ThirExpression
    {
        override val errorKind: ThirKind get() = ThirKind.Nothing
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
    
    /**
     * Constants represents bindings evaluated at compile-time.
     */
    data class Const(
        override val id: UUID,
        override val name: String,
        val kind: ThirKind,
        var def: ConstDef?,
    ) : ThirDeclaration
    
    data class ConstDef(
        val value: ThirExpression.Canonical,
    )
    
    /**
     * Fields represents a value or attribute associated with an instance of a type.
     */
    data class Field(
        override val id: UUID,
        override val name: String,
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
        val fieldIds: List<UUID>,
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
        val kind: ThirKind,
        var def: VariableDef?,
    ) : ThirDeclaration
    
    data class VariableDef(
        val value: ThirExpression,
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
    kind = ThirKind.Value(ThirType.Int32),
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Int32(0),
    )
)
val a = ThirDeclaration.Const(
    id = UUID.randomUUID(),
    name = "a",
    kind = ThirKind.Value(ThirType.Int32),
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Int32(0),
    )
)

// Here is some code samples involving generics:
/*
struct Ptr[Type: __builtin_type]
{
    // builtin
}

struct Array[Element: __builtin_type, size: Int32]
{
    val data: Ptr[Type = Element]
    val test = size
}

val collection: Array[Bool, 17] = magic()
val foo = 42
val bar: Array[Str, foo] = magic()
*/
val ptrType = ThirDeclaration.TypeParameter(
    id = UUID.randomUUID(),
    name = "Type",
    kind = ThirKind.Type,
    def = ThirDeclaration.TypeParameterDef(
        default = null,
    )
)
val ptr = ThirDeclaration.Structure(
    id = UUID.randomUUID(),
    name = "Ptr",
    typeParameterIds = listOf(ptrType.id),
    fieldIds = emptyList(),
    def = ThirDeclaration.StructureDef(null),
)

val arrayElement = ThirDeclaration.TypeParameter(
    id = UUID.randomUUID(),
    name = "Element",
    kind = ThirKind.Type,
    def = ThirDeclaration.TypeParameterDef(
        default = null,
    )
)
val arraySize = ThirDeclaration.TypeParameter(
    id = UUID.randomUUID(),
    name = "size",
    kind = ThirKind.Value(ThirType.Int32),
    def = ThirDeclaration.TypeParameterDef(
        default = null,
    )
)
val arrayData = ThirDeclaration.Field(
    id = UUID.randomUUID(),
    name = "data",
    kind = ThirKind.Value(ThirType.Structure(ptr.id, listOf(ThirExpression.Type(ThirType.TypeParameterRef(arrayElement.id))))),
    def = ThirDeclaration.FieldDef(
        default = null,
    )
)
val arrayTest = ThirDeclaration.Field(
    id = UUID.randomUUID(),
    name = "test",
    kind = ThirKind.Value(ThirType.Int32),
    def = ThirDeclaration.FieldDef(
        default = ThirExpression.Load(arraySize.id, ThirKind.Value(ThirType.Int32)),
    )
)
val array = ThirDeclaration.Structure(
    id = UUID.randomUUID(),
    name = "Array",
    typeParameterIds = listOf(arrayElement.id, arraySize.id),
    fieldIds = listOf(arrayData.id, arrayTest.id),
    def = ThirDeclaration.StructureDef(null),
)

val collection = ThirDeclaration.Const(
    id = UUID.randomUUID(),
    name = "collection",
    kind = ThirKind.Value(ThirType.Structure(array.id, listOf(ThirExpression.Type(ThirType.Bool), ThirExpression.Int32(17)))),
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Type(ThirType.Str), // NOTE: Just a placeholder, this is a stand-in for a legal value.
    )
)
val foo = ThirDeclaration.Const(
    id = UUID.randomUUID(),
    name = "foo",
    kind = ThirKind.Value(ThirType.Int32),
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Int32(42),
    )
)
val bar = ThirDeclaration.Const(
    id = UUID.randomUUID(),
    name = "bar",
    kind = ThirKind.Value(ThirType.Structure(array.id, listOf(ThirExpression.Type(ThirType.Str), ThirExpression.Int32(42)))),
    def = ThirDeclaration.ConstDef(
        value = ThirExpression.Type(ThirType.Str), // NOTE: Just a placeholder, this is a stand-in for a legal value.
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
