package com.github.derg.transpiler.experimental.toy_solver

import com.github.derg.transpiler.source.hir.*
import java.util.*

/*
```derg
// Declaring a new constant
val value = 42
val whatever: String = type_info[false](ArrayList[Bool])

// Declaring a new interface type
trait Collection
{
    fun is_empty() -> Bool
    fun size() -> Int
    fun clear()
}

trait Iterable[T: type] // Declaring a new type parameter
{
    fun iterator() -> Iterator[T]
}

// Declaring a new record type
class ArrayList[T: type]
{
    // Declaring a new field
    var size = 0
    var capacity = 0
    var data: Pointer[T]

    // Declaring a new method
    fun resize() { ... }

Iterable[T]:
    fun iterator() -> Iterator[T] { ... }

Collection[T]:
    fun is_empty(): Bool { return size == 0 }
    fun size(): Int { return size }
    fun clear(): { ... }
}

// Declaring a new function
fun type_info[pretty_print: Bool](input: type) -> String
{
    // Declaring a variable
    val name = input.name

    if pretty_print
        return "$name\n\t..."
    else
        return "$name(...)"
}

fun stuff(value: Int) // Declaring a new parameter
{
    ...
}

fun deduced_return() -> auto // Deduced return type to be `Int`
{
    return 0
}
```
*/
interface Hir
{
    sealed interface Kind
    data object KAuto : Kind
    data object KNothing : Kind
    data object KType : Kind
    data class KValue(val type: Expr) : Kind
    
    sealed interface Expr
    {
        val id: UUID
    }
    
    data class IntLiteral(override val id: UUID, val value: Int) : Expr
    data class FloatLiteral(override val id: UUID, val value: Float) : Expr
    data class BoolLiteral(override val id: UUID, val value: Boolean) : Expr
    data class StringLiteral(override val id: UUID, val value: String) : Expr
    data class NameReference(override val id: UUID, val name: String, val typeArguments: List<Argument>) : Expr
    data class If(override val id: UUID, val predicate: Expr, val thenBranch: Expr, val elseBranch: Expr) : Expr
    data class Is(override val id: UUID, val value: Expr, val type: NameReference) : Expr
    data class Call(override val id: UUID, val callee: Expr, val arguments: List<Argument>) : Expr
    data class FieldAccess(override val id: UUID, val receiver: Expr, val field: NameReference) : Expr
    
    sealed interface Statement
    data class Declare(val id: UUID, val name: String, val kind: Kind?, val value: Expr) : Statement
    data class Assign(val id: UUID, val binding: Expr, val value: Expr) : Statement
    data class Return(val id: UUID, val value: Expr) : Statement
    
    data class Argument(val name: String?, val value: Expr)
    
    data class TypeParameterDecl(
        val id: UUID,
        val name: String,
        val kind: Kind?,
        val default: Expr?,
    )
    
    data class ParameterDecl(
        val id: UUID,
        val name: String,
        val kind: Kind?,
        val default: Expr?,
    )
    
    data class FieldDecl(
        val id: UUID,
        val name: String,
        val kind: Kind?,
        val default: Expr?,
    )
    
    data class MethodDecl(
        val id: UUID,
        val name: String,
        val typeParameters: List<TypeParameterDecl>,
        val parameters: List<ParameterDecl>,
        val returnKind: Kind,
        val statements: List<Statement>,
    )
    
    data class TraitDecl(
        val id: UUID,
        val name: String,
        val typeParameters: List<TypeParameterDecl>,
        val methods: List<MethodDecl>,
    )
    
    data class StructureDecl(
        val id: UUID,
        val name: String,
        val typeParameters: List<TypeParameterDecl>,
        val fields: List<FieldDecl>,
        val methods: List<MethodDecl>,
        val constructor: FunctionDecl,
        val implements: List<ImplementDecl>,
    )
    
    data class UnionDecl(
        val id: UUID,
        val name: String,
        val variants: List<NameReference>,
    )
    
    data class FunctionDecl(
        val id: UUID,
        val name: String,
        val typeParameters: List<TypeParameterDecl>,
        val parameters: List<ParameterDecl>,
        val returnKind: Kind,
        val statements: List<Statement>,
    )
    
    data class ImplementDecl(
        val id: UUID,
        val trait: NameReference,
        val methods: List<MethodDecl>,
    )
}

interface Mir
{
    sealed interface Kind
    data object KType : Kind
    data class KValue(val type: Type) : Kind
    
    sealed interface Type
    data object TBool : Type
    data object TInt : Type
    data object TFloat : Type
    data object TString : Type
    data class TUnion(val variants: Set<Type>) : Type
    data class TRecord(val fields: Map<Int, Type>) : Type
    data class TFunction(val parameters: List<Type>, val returnType: Type) : Type
    
    sealed interface Expr
    data class IntLiteral(val value: Int) : Expr
    data class FloatLiteral(val value: Float) : Expr
    data class BoolLiteral(val value: Boolean) : Expr
    data class StringLiteral(val value: String) : Expr
    data class If(val predicate: Expr, val thenBranch: Expr, val elseBranch: Expr) : Expr
    data class Call(val callee: Expr, val arguments: List<Expr>) : Expr
    data class FieldAccess(val receiver: Expr, val fieldId: Int) : Expr
    
    sealed interface Statement
    data class Declare(val id: Int, val kind: Kind?, val value: Expr) : Statement
    data class Assign(val binding: Expr, val value: Expr) : Statement
    data class Return(val value: Expr) : Statement
    
    data class TypeParameterDecl(
        val name: String,
        val kind: Kind,
        val default: Expr?,
    )
    
    data class ParameterDecl(
        val name: String,
        val kind: Kind,
        val default: Expr?,
    )
    
    data class FieldDecl(
        val name: String,
        val kind: Kind,
        val default: Expr?,
    )
    
    data class MethodDecl(
        val name: String,
        val typeParameters: List<TypeParameterDecl>,
        val parameters: List<ParameterDecl>,
        val returnKind: Kind,
        val statements: List<Statement>,
    )
    
    data class TraitDecl(
        val name: String,
        val typeParameters: List<TypeParameterDecl>,
        val methods: List<MethodDecl>,
    )
    
    data class ClassDecl(
        val name: String,
        val typeParameters: List<TypeParameterDecl>,
        val fields: List<FieldDecl>,
        val methods: List<MethodDecl>,
        val constructor: HirDeclaration.FunctionDecl,
        val implements: List<ImplementDecl>,
    )
    
    data class UnionDecl(
        val name: String,
        val variants: List<Int>,
    )
    
    data class FunctionDecl(
        val name: String,
        val typeParameters: List<TypeParameterDecl>,
        val parameters: List<ParameterDecl>,
        val returnKind: Kind,
        val statements: List<Statement>,
    )
    
    data class ImplementDecl(
        val trait: Int,
        val methods: List<MethodDecl>,
    )
}
