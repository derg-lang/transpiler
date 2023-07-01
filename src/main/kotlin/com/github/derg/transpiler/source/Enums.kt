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
 * The kind of mutability determines what is permitted regarding the data the variable holds. The kind specifies the
 * variable mutability level, restricting the variable from never-changing, to fully mutable.
 */
enum class Mutability
{
    /**
     * The variable is considered a constant. The value can never change, and the properties associated with the value
     * cannot change either. The variable is considered deeply immutable, and the user is guaranteed that its memory
     * space will remain unchanged.
     *
     * All forms of mutable operations on immutable objects is strictly forbidden. Only non-mutation operations may be
     * performed on such objects. Most builtin types (i.e. integers, booleans) are immutable. Any properties explicitly
     * marked as mutable may be mutated, however.
     *
     * All variables which are immutable and contains no explicitly mutable properties may be inlined, or statically
     * computed to other values at compile time, wherever possible.
     */
    IMMUTABLE,
    
    /**
     * The variable may be mutated by invoking mutating functions or writing different values to the variable
     * properties.
     *
     * The variable itself may be subject to other constraints preventing assigning new values to it, however. see
     * [Assignability] for more information.
     */
    MUTABLE,
}

/**
 * Variables may be re-assigned a different value in certain cases, for example during the assignment statement. In many
 * cases, variables cannot be re-assigned due to their mutability constraints, or their special properties. The
 * assignability property determines how assigning new values to a variable behaves, or whether it is legal in the first
 * place.
 */
enum class Assignability
{
    /**
     * The variable is marked as a constant and cannot be re-assigned for any purpose. It may still be mutated, if it is
     * marked as mutable. Phrased differently, a constant is a variable which can never be re-assigned after creation;
     * it will always point to the same instance as it was created with.
     */
    CONSTANT,
    
    /**
     * The variable may be re-assigned as desired. The implementation of re-assignment determines how the memory is
     * updated. For trivial types, the entire memory region is overwritten, although certain types may define a more
     * complicated assignment scheme, where memory is not fully overwritten.
     */
    ASSIGNABLE,
    
    /**
     * The variable is a reference type and points to some other location in memory. The variable itself may not be
     * re-assigned to point to some other location in memory, but instead allows the target location to be updated
     * instead.
     */
    REFERENCE,
    
    /**
     * The variable is a pointer type, which may or may not be pointing at a valid object in memory.
     */
    POINTER,
}

/**
 * Defines the properties a value is expected to have when passed into a function as a parameter. Depending on how the
 * value is expected to be used, certain attributes and requirements will be imposed on the value.
 *
 * See https://github.com/hsutter/708 for further information.
 */
enum class Passability
{
    /**
     * Marks the value as an input-only value. Within a function, the variable cannot be modified in any way, shape, or
     * form. The value is considered immutable within the function. The value must be initialized before it can be
     * passed into the function, and may be a rvalue.
     */
    IN,
    
    /**
     * The value must be mutated in some manner within a function using it. At least one path within the function must
     * have a non-const usage of the variable. The value must be a non-const lvalue.
     */
    INOUT,
    
    /**
     * Marks the value as an output-only value. The value must be initialized or assigned to within the function, and
     * cannot be read from before it has been provided a value. The value must be non-const variable, and cannot be a
     * rvalue.
     *
     * Within the function using the parameter, the variable is considered mutable. Outside the function, the variable
     * is assigned either mutable or immutable, and will be treated as such.
     */
    OUT,
    
    /**
     * Consumes the provided value, meaning it cannot be accessed anymore. Any value passed into a function call using
     * this option makes the value lost on the outside of the function. Effectively, this is a way to pass ownership of
     * any value into and out of a function.
     */
    MOVE,
}

// TODO: Consider the following code, how will destroying variables work?
/*
fun read_data(out rows list<data>): error -> bool
{
    // Implementation omitted
}

fun do_work(): error
{
    val mut list<data> rows
    while read_data(rows) ! // How will `rows` be destroyed when an error occurs?
        process_data(rows)
}

An approach of solving this issue is to default-construct the value when it is left hanging in an uninitialized state.
This means the object will always be *valid* albeit not populated with meaningful data until it is either initialized by
an `out` parameter function call, or initialized explicitly.

The caller will always run the destructor, cleaning up the local instance. The callee will only clean up its instance if
it has actually moved anything into the `out` variable, passing the fresh one to the caller for cleanup. This means that
if an error occurs before the second instance is made, the caller cleans up the mess - otherwise, the callee cleans up
one instance, and the caller the other.
*/
