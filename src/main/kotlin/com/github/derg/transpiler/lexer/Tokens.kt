package com.github.derg.transpiler.lexer

interface Foo
{
    val word: String
}

sealed class Token

/**
 * All symbols such as variables, functions, type, namespaces, packages, etc. must be given a unique name. The
 * identifier token represents any such object by [name], although the token does not hold information determining which
 * object type it is applicable to.
 */
data class Identifier(val name: String) : Token()

/**
 * Source code requires the use of keywords to specify what various objects are, and how the logic of the program flows.
 * The token represents the [type] of keyword.
 */
data class Keyword(val type: Type) : Token()
{
    enum class Type(override val word: String) : Foo
    {
        AUTO("auto"),
        DEFAULT("default"),
        ELSE("else"),
        FALSE("false"),
        FOR("for"),
        FUN("fun"),
        IF("if"),
        IN("in"),
        MUT("mut"),
        PUB("pub"),
        TRUE("true"),
        TYPE("type"),
        VAL("val"),
        VAR("var"),
        WHILE("while"),
    }
}

/**
 * Relation between code components determines the program form and how various components interact with each other. The
 * structure defines the lifetimes and scopes of various objects, as well as the boundaries between one object to
 * another. The token represents the [type] of the structural component.
 */
data class Structure(val type: Type) : Token()
{
    enum class Type(override val word: String) : Foo
    {
        ARROW("->"),
        CLOSE_BRACE("}"),
        CLOSE_BRACKET("]"),
        CLOSE_PARENTHESIS(")"),
        COLON(":"),
        COMMA(","),
        OPEN_BRACE("{"),
        OPEN_BRACKET("["),
        OPEN_PARENTHESIS("("),
        PERIOD("."),
        SEMICOLON(";"),
    }
}

/**
 *
 */
data class Operator(val type: Type) : Token()
{
    enum class Type(override val word: String, val priority: Int) : Foo
    {
        // Assignment operators
        ASSIGN("=", -1),
        ASSIGN_DIVIDE("/=", -1),
        ASSIGN_MINUS("-=", -1),
        ASSIGN_MODULO("%=", -1),
        ASSIGN_MULTIPLY("*=", -1),
        ASSIGN_PLUS("+=", -1),
        DECREMENT("--", -1),
        INCREMENT("++", -1),
        
        // Arithmetic operators
        DIVIDE("/", 2),
        MINUS("-", 1),
        MULTIPLY("*", 2),
        PLUS("+", 1),
        MODULO("%", 2),
        
        // Comparison operators
        EQUAL("==", -1),
        GREATER(">", -1),
        GREATER_EQUAL(">=", -1),
        LESS("<", -1),
        LESS_EQUAL("<=", -1),
        NOT_EQUAL("!=", -1),
        THREE_WAY("<=>", -1),
        
        // Logical operators
        AND("&&", -1),
        NOT("!", -1),
        OR("||", -1),
        XOR("^^", -1),
    }
}

/**
 * The token holds a raw numerical literal of a specific [value] and optional [type].
 */
data class Numeric(val value: Number, val type: String?) : Token()

/**
 * The token holds a raw textual literal of a specific [value] and optional [type].
 */
data class Textual(val value: String, val type: String?) : Token()

/**
 * Returns the raw string representing the token in source code.
 */
val Token.raw: String
    get() = when (this)
    {
        is Identifier -> name
        is Keyword    -> type.word
        is Numeric    -> value.toString()
        is Operator   -> type.word
        is Structure  -> type.word
        is Textual    -> value
    }
