package com.github.derg.transpiler.source

/**
 * Encapsulation of data and functionality is achieved through visibility. Certain functionality, behavior, and state
 * may be concealed behind an abstraction, where anything outside the abstraction cannot see the internal behavior or
 * state.
 *
 * Visibilities are hierarchical in nature. Everything which is accessible under the public domain is also accessible by
 * everything which could access protected elements, and similarly for everything which has access to private elements.
 */
enum class Visibility
{
    /**
     * The object is accessible to everything outside the package; it may be used by any part of the source program,
     * including third-party consumers.
     */
    EXPORTED,
    
    /**
     * The object will be visible to all modules within the same package. Modules contained within other packages will
     * not be able to access the object.
     */
    PUBLIC,
    
    /**
     * The object is accessible to everything within the same module as the object was declared in. The object will not
     * be visible to anything outside the current module.
     */
    PROTECTED,
    
    /**
     * The object is only accessible to the current type in which the object was declared. For example, a private
     * variable may only be accessed by the object where the variable was declared.
     */
    PRIVATE,
}

/**
 * Mutability determines whether a type can be internally modified or not. If a type is considered immutable, then in
 * the context it is used, it cannot be changed by a developer. If a list is passed in an immutable context, the
 * developer cannot. for example, append new elements to it - this would only be possible if the list was passed in a
 * mutable context.
 *
 * Note that immutable does not mean that a type will never change, but merely indicates that the type cannot be changed
 * in the current context. Some types such as integers, booleans, and other builtin types are naturally immutable. Their
 * inner state cannot be changed.
 *
 * Mutability only applies to a type, not the container holding a value. This means a variable might store an immutable
 * value, but the variable itself can be assigned another immutable value.
 */
enum class Mutability
{
    /**
     * The type is mutable in the current context, and may be mutated by either directly assigning values to its fields
     * or invoking its mutating methods. The type may be deeply mutable, if the fields are declared mutable. Any mutable
     * field may be mutated, either through its fields or mutating methods.
     */
    MUTABLE,
    
    /**
     * The type is immutable in the current context; the developer is not permitted to write any values to its fields,
     * nor invoke any mutating methods on it. The value is deeply immutable as well; none of the fields may be mutated
     * either.
     */
    IMMUTABLE,
}

/**
 * Variables and fields are commonly assigned new values, although in some conditions this is not permitted. The
 * assignability determines in which contexts the binding may be assigned a new value, and how it should be done. In
 * some situations, the binding itself will point to a new value. For references, an indirection takes place, where
 * another binding may be reassigned instead.
 */
enum class Assignability
{
    /**
     * The variable or field is marked as final, meaning it can never be reassigned another value. Once the object has
     * been granted a value, that value can never be replaced with another value. If the type of the object allows it,
     * however, the value itself may be mutated.
     */
    FINAL,
    
    /**
     * The variable or field may be reassigned as desired by the developer. Type restrictions still applies, meaning
     * that a developer may only assign compatible types to the object.
     */
    ASSIGNABLE,
    
    /**
     * The variable is a reference type and points to some other location in memory. The variable itself may not be
     * re-assigned to point to some other location in memory, but instead allows the target location to be updated
     * instead.
     */
    REFERENCE,
}

/**
 * Parameters for functions, methods, or other callable objects represent a value from outside the object. This value
 * may only be passed into the function in specific manners, represented with the passing category. This property
 * specifies the intent of the developer, detailing how the parameter is intended to be used, rather than how it is
 * passed into the object.
 */
enum class Passability
{
    /**
     * Marks the parameter as a read-only value. The value cannot be modified in any way within the callable object. All
     * in parameters must be fully immutable; mutating an in parameter is strictly forbidden. There are no restrictions
     * on what values may be passed as an in parameter, provided the type of the value matches the type expected by the
     * parameter.
     */
    IN,
    
    /**
     * Marks the parameter as a read-write value. The value must be granted a value at least once within the callable
     * object. The value must be assigned to the parameter before the parameter is used in any way, and the callable is
     * not permitted to return or raise an error before the parameter has been granted a value. The value will not be
     * destroyed when leaving the callable object scope.
     */
    OUT,
    
    /**
     * Marks the parameter as an owned value, transferring the ownership from outside the callable object, to the object
     * itself. The callable will be responsible for cleaning up the resources used by the value once it leaves the
     * callable object scope.
     */
    MOVE,
    
    /**
     * Marks the parameter as a borrowed value. The value cannot be reassigned from within the callable object, but it
     * may be mutated if the type of the value permits that. Values which are borrowed are not destroyed when they leave
     * the callable object scope, and ownership of them cannot be granted either.
     */
    BORROW,
}

/**
 * How a captured error should be handled, when handled inline in an expression. The captured error will be accessible
 * as a temporary binding, allowing the developer to transform it in any way they need before the program continues.
 */
enum class Capture
{
    /**
     * The error is transformed into a value, and used instead of the success value which would otherwise be used. The
     * computed value is used as a default value when an error has occurred.
     */
    HANDLE,
    
    /**
     * The error is transformed into a value, which is then returned from the callable object as an error. This
     * operation is equivalent to capturing the error, transforming it, and then raising it.
     */
    RAISE,
    
    /**
     * The error is transformed into a value, which is then returned from the callable object as a success. This
     * operation is equivalent to capturing the error, transforming it, and then returning it.
     */
    RETURN,
}

/**
 * The various symbols which are considered builtin. When parsing source code, these are the character sequences which
 * have special meanings. Commonly for all of them is that identifiers cannot be the same as the [symbol] string itself.
 * These values are typically considered reserved.
 */
enum class Symbol(val symbol: String)
{
    // Keywords
    AUTO("auto"),
    BORROW("borrow"),
    DEFAULT("default"),
    ELSE("else"),
    EXPORTED("exported"),
    FALSE("false"),
    FOR("for"),
    FUN("fun"),
    IF("if"),
    IN("in"),
    MODULE("module"),
    MUTABLE("mut"),
    MOVE("move"),
    OUT("out"),
    PRIVATE("private"),
    PROTECTED("protected"),
    PUBLIC("public"),
    REFERENCE("ref"),
    RETURN("return"),
    RAISE("raise"),
    TRUE("true"),
    TYPE("type"),
    USE("use"),
    VALUE("val"),
    VARYING("var"),
    WHEN("when"),
    WHILE("while"),
    
    // Structural components
    ARROW("->"),
    CLOSE_BRACE("}"),
    CLOSE_BRACKET("]"),
    CLOSE_PARENTHESIS(")"),
    COLON(":"),
    COMMA(","),
    EXCLAMATION("!"),
    OPEN_BRACE("{"),
    OPEN_BRACKET("["),
    OPEN_PARENTHESIS("("),
    PERIOD("."),
    QUESTION("?"),
    SEMICOLON(";"),
    
    // Assignment operators
    ASSIGN("="),
    ASSIGN_DIVIDE("/="),
    ASSIGN_MINUS("-="),
    ASSIGN_MODULO("%="),
    ASSIGN_MULTIPLY("*="),
    ASSIGN_PLUS("+="),
    
    // Arithmetic operators
    DIVIDE("/"),
    MINUS("-"),
    MULTIPLY("*"),
    PLUS("+"),
    MODULO("%"),
    
    // Comparison operators
    EQUAL("=="),
    GREATER(">"),
    GREATER_EQUAL(">="),
    LESS("<"),
    LESS_EQUAL("<="),
    NOT_EQUAL("~="),
    THREE_WAY("<=>"),
    
    // Logical operators
    AND("&&"),
    NOT("~"),
    OR("||"),
    XOR("^^"),
}
