# Type System

The type system of a programming language is perhaps one of the strongest advantages for writing large pieces of
software. With a strong type system, a developer can rest assured that the compiler can detect and report a large number
of erroneous programs, without the developer needing to run the program in the first place. The compiler can deduce that
a function cannot be called with some values, that certain values are unused, errors are not handled, and much more.

In Derg, the type system allows the developer to express their intent clearly and concisely. All values are associated
with a type, which determines how the value can and cannot be used. The shape of the data, visibility, mutability,
parameter passing rules, and so on influences the possible ways values may be passed around.

## Encoding types

// TODO: Write me

### Data structures

In order to store information, the shape of the information must be defined. All such information is provided by the
data structure type. Data structures may contain any number of fields of information, each field being of an arbitrary
type.

A data structure may be declared in the following manner:

```derg
struct MyType
{
    val field_one: Int32
    val field_two: Float64
    // And so on...
}
```

The actual layout of the data structure in memory on the computer is implementation-defined by the compiler.

Data structures can be expressed in multiple manners:

```derg
Int32               // Builtin data types behave just like user-declared types.
MyType              // Custom types are just the same as builtins.
Map[Int32, Float64] // Types may be parameterized with additional type information.
Array[MyType, 5]    // Generics may contain expressions, too.
```

### Callables

Not all aspects of a program consists of data, however. Sometimes the most appropriate solution is to pass a function of
some kind as a parameter, or to store one for later use. Types of this object are referred to as a callable type.

These types may be expressed in various manners:

```derg
fun()                       // The most simple way of representing a callable without input parameters or returns.
fun(Int32, Bool)            // A callable which takes in two parameters.
fun() -> Int32              // Callables are permitted to return success values.
fun(): Error                // They may also return error values, which must be properly handled by the caller.
fun(MyType): Error -> Bool  // Callables may contain any combination of parameters and return values and errors.
```

Note that it is not possible to directly specify that a callable should return another callable. In order to specify
that a callable can return another callable, [Alias types](#aliases) should be used. An example of returning a callable
from another can be done in the following manner:

```derg
alias MyCallable = fun(): Error -> Int32 // Aliasing the callable we want to return from another callable type.

fun() -> MyCallable // This is now a permitted type, and is not ambiguous.
```

### Concepts

// TODO: Write me

### Unions

A common need in programming languages, is the need to express that a value is one of an exact number of types. The
difference between a union and a concept, is that the concept is an open set of types - it can be extended with new
types when new code is written. A union is a fixed set of a specific number of types, however. It cannot be extended by
other source code.

These types may be expressed in the following manner:

```derg
union JsonValue = JsonObject + JsonArray + JsonNull + Float64 + Int64 + String + Bool
```

When a value is given a union type, it must take the type of exactly one of the types making it up. Unions support smart
casting - the type of the value may be deduced by eliminating the types it is not, or by explicitly state which type it
is:

```derg
union Data = Int64 + Float64 + Bool

fun example(data: Data)
{
    if data.is[Int64]()
    {
        // `data` is `Int64` here
    }
    else
    {
        // `data` is `Float64 + Bool` here
    
        if data.is[Bool]()
            return
    
        // `data` is `Bool` here
    }
}
```

Note that smart-casting may not always be possible. When a field within an object is accessed, smart-casting is only
possible if the field does not change as a side effect. As an example, consider the following example:

```derg
union Data = Int64 + Float64 + Bool

struct Type
{
    var data: Data
}

fun Type.mutate()
{
    // Implementation omitted
}

fun example(value: Type)
{
    if value.data.is[Int64]()
    {
        value.mutate()
        // `value.data` is not necessarily `Int64` at this point
    }
    else
    {
        // `value.data` is guaranteed to be `Float64 + Bool` at this point
    }
}
```

### Aliases

// TODO: Write me

## Visibility modifiers

The visibility of a symbol influences where the symbol may be used in source code. The visibility of a symbol details
the effective scope which can access it by name - a scope which is outside the visibility range specified by the
developer cannot access said symbol by name.

Visibility modifiers come in four different levels:

- **EXPORTED**: The symbol is accessible to everything outside the current package. It may be used in any part of the
  source code, including third-party code. Any code which should be shared between different packages, must be declared
  as exported - this is the only visibility level which allows third-party usage; libraries should declare all symbols
  forming the actual interface as `exported`.
- **PUBLIC**: The symbol is accessible to all modules within the current package. This visibility layer allows all
  modules within the package to access the symbol by name. The public visibility should be used for symbols which are
  used throughout the module, but should not be exposed to the consumers of the code. For an application package,
  symbols such as loggers, database connections, and similar might be declared `public`.
- **PROTECTED**: The symbol is accessible to everything within the same module as the object was declared in. The object
  will not be visible to anything outside the current module. This is useful for implementing behavior for the module,
  but should not form part of the module's interface. Such symbols are declared `protected`.
- **PRIVATE**: The symbol is only accessible by name to the segment it is declared in. This is useful to hide
  implementation details which are not relevant to other parts of the software. Examples include simple helper functions
  and utilities used to implement the actual behavior (i.e. transforming data from one struct to another) or other
  low-level handling. Such symbols are typically declared `private`.

Example of different visibilities:

```derg
export  val CONSTANT_A = 1 // Accessible everywhere.
public  val CONSTANT_B = 2 // Accessible only in the same package.
protect val CONSTANT_C = 3 // Accessible only in the same module.
private val CONSTANT_D = 4 // Accessible only in the same segment.
```

The visibility modified does not imply accessibility. A symbol which is invisible from one part of the codebase may
access it via a different part of the codebase which does have access to it. This may for example be the case where
symbols are passed as parameters or otherwise stored in variables, which enables symbols to cross module boundaries
while respecting visibility rules.

An example of visibility rules being bypassed can be seen here:

```derg
// Module A
private fun hidden() {}
public fun snitch() -> fun() { return hidden }

// Module B
public fun using_hidden()
{
    snitch()() // This invoked the function returned by `snitch`, namely the `hidden` function
}
```

## Mutability modifiers

In software, state is common and often everywhere. A program in which state changes frequently is typically harder to
reason around, but if no state is permitted to change the programming language quickly becomes less ergonomic. In Derg,
mutability is a key concept; the developer is granted powerful tools to express the intentions in a clean and
comprehensible manner.

Mutability in Derg revolves around two key concepts, assignments and mutations. Assignments refer to changing which
value is bound to a specific variable at any given time, whereas mutation refers to mutating a value *through* a
binding. Depending on how a variable is declared, and the type of the variable, assignment and/or mutation may be
prohibited.

### Bindings

When a developer declares a variable or struct field, the symbol must be declared as final or non-final. Any variable
which is declared final, cannot be re-assigned. If the variable is declared within a function body, then every time the
function is invoked a different instance of that variable may be granted a different value.

A non-final variable can be altered as many times as needed, however. Typically, a running sum when computing the sum of
all numbers in a list would be non-final. The sum would continuously be assigned the new current total with each
iteration. However, allowing variables to be assigned different values can quickly make a program harder to follow. As
such, non-final variables should be used sparingly.

A final variable is declared using `val`, whereas a non-final one is declared with `var`. Examples of such variables can
be seen here:

```derg
struct MyData
{
    val name: String // Final, cannot be re-assigned once given a value.
    var age: Int32   // Non-final, can be re-assigned at any time.
} 
```

Note that final and non-final bindings applies to the symbol itself, not the value contained within the variable. A
variable can be final or non-final, independent of what the actual mutability of the value is.

### Mutation

Values are different from variables and fields, in the sense they do not point to a specific location in memory, but
rather represent some data. This data can be considered logically constant, or be permitted to be mutated.

Data which is logically constant, cannot be modified in any way through the binding pointing to it. For example, a list
which is declared constant, cannot be appended to in the current context - it may be changed elsewhere in the program,
but in this context it is effectively a read-only value. Mutable data, however, can be modified as much as the developer
likes. A mutable list can be appended to, elements can be removed, or even updated if the element type permits such.

The mutation of values has far-reaching consequences; the developer is able to express deeply mutable structures, or
shallowly mutable structures on-demand. For example, a developer will be able to express that a function can take in a
mutable list, but the elements within the list cannot be mutated. Or alternatively, the developer expresses that the
list *cannot* be mutated, but the elements within it *can* be.

Mutation applies to the type of the value itself, and may be declared in the following manner:

```derg
struct MyData
{
    val my_const: List[Int]   // A list which cannot be mutated, the contents of the list will never change.
    val my_mut: mut List[Int] // A list which can be modified, its contents is permitted to change.
} 
```

Note that there is a subtle but important difference between mutability of values, versus assigning new values to a
binding. By reassigning a variable, the old value is destroyed before moving the new value in place. This has various
performance impacts; for large resources, creating the new value may be a costly operation. Mutating the data in-place
is typically a more efficient solution.

## Value passing

Unlike most other programming languages, Derg does not focus on *how* a parameter is passed into or out of a function,
but rather on the *why*. The developer will not have to worry about the details of whether a value is considered an
l-value, r-value, pointer, or similar. Instead, developers can focus on the intentions of the program, by explicitly
stating what the purpose of the parameter is instead.

Values may be passed into and out of functions in one of multiple manners:

- **BORROW**: The most common way to pass values around is as a borrowed value. If the developer needs to make changes
  to a value as a side effect within a function, or provide access to a value in a struct, the parameter or return value
  can be declared as `borrow`. In this case, the binding is an alias to the value passed into or out of the function.
  The binding itself cannot be assigned a new value, but if the type of the parameter permits so, it may be mutated as a
  side effect.
- **MOVE**: The developer can transfer ownership of a value into or out of a function, by declaring a parameter
  as `move`. When doing so, the value passed into the function can no longer be used on the outside, as it does no
  longer exist there. The function which accepted the value, will from now on be responsible for managing its lifetime.
- **COPY**: If the developer does not want to pass ownership, nor deal with lifetime rules, they may declare the value
  as `copy` instead. Any type which is copyable, can be passed in this manner; for types which are not trivially
  copyable, the developer may have to write the functionality for copying the type first.

When a value is passed as `borrow`, the developer will have to take the [lifetime](resource-management.md#lifetimes) of
the value into consideration. A value cannot outlive the scope it is declared in, nor can the memory the value resides
in be invalidated in any way, shape, or form. Note that values owned by a scope cannot be borrowed to a scope that
outlives the original scope; this means that functions cannot return borrows to local variables.

A value which is moved, cannot be accessed in the previous scope. For example, a value passed into a function by `move`,
is now inaccessible in the function it was originally declared in. Any access to the value through its previous binding
is a compile-time error. As [ownership](resource-management.md#ownership) is transferred to another scope, once the
value goes out of the new scope it will be destroyed as usual.

Note that passing values by copy, does not mean a bitwise identical copy is passed. All types which are copyable, must
define a copy function, detailing how the data can be copied. The types themselves are responsible for defining what
constitutes a valid copy. Typically, a copy means a deep copy, where nested data is copied as well, rather than just the
reference to the nested data.

Values may be passed into functions in the following manners:

```derg
fun my_function(borrow input: Int32) // Passes the value as a borrowed value. 
fun my_function(move input: Int32)   // Passes overship of the value into the function.
fun my_function(copy input: Int32)   // Passes a copy of the value into the function.
```

Values may be returned from functions in the following manners:

```derg
fun my_function() -> borrow Int32 // Returns a borrowed value from the function.
fun my_function() -> move Int32   // Returns a value together with its ownership from the function.
fun my_function() -> copy Int32   // Returns a copy of some inner value from the function.
```

### Borrowing

// TODO: Write me.

### Moving values

Moving a value passes the ownership of the value from the caller to the callee. This means that the caller is no longer
responsible for freeing the value. The destructor of the value will only be called in the callee instead, preserving the
guarantee that all values are eventually destroyed.

Values should typically be moved only when the semantics of the program demands that ownership must change. This is the
case when the lifetime of a value must be extended beyond the scope it was created in. Whenever a value is passed into
another scope as a `move`, the lifetime of the value is guaranteed to be as long as the new scope's lifetime.

### Copying values

// TODO: Write me.

## Error handling

Traditional programming languages are often based on a form of exception mechanism, return value, or some form
of `Result` type. These approaches have advantages and disadvantages, typically in some way of being too intrusive and
unergonomic, easy-to-forget to handle, or some other variant thereof. Derg seeks to use a `Result` approach, but rather
than repeating the mistakes of other languages with this approach, the error path will be a first-class citizen.

Typically, functions can fail if one or more invariants are violated, and the type system is unable to capture the error
to make it impossible. This can for example be the case when the developer wants to access a value stored in a map of
some sort, and want to access a non-existing key. As we cannot easily or at all detect such errors, we need an
alternative way to represent this error. Other languages opt in for a `null`, `undef`, or `Option` variant, but Derg can
express this violation in a far more powerful feature: the error track.

### Error track

All functions in Derg may be declared with a return value, an error value, both, or neither. A function which does not
return an error, is not permitted to fail for any reason. All errors *must* be handled inside the function, and it is
not permitted to propagate errors upwards. On the contrary, a function which does declare an error value, is permitted
to pass on errors of that type only.

The advantage of handling errors as their own track, is that functions can more easily be passed around as parameters
themselves. Certain functions take other functions as parameters, for example a `map` function for collections. Such
functions may expect the operation to always succeed, or may provide specialized implementations for functions which can
fail. In such cases, if we provide a function which never fails, a developer might be able to optimize their
implementation to be considerably more efficient or safe. Or, if only a fallible function can be provided as a
parameter, the developer can still pass a function which never fails; the error case is just never triggered in that
case.

Developers are thus able to re-use their implementations for a greater number of functions, avoiding the function color
problem. Functions which never fails, can *always* be used in place of a fallible function, assuming the function
signatures are otherwise similar.

A function can be declared to return errors in the following manner:

```derg
fun my_function(): Error // This function is permitted to raise errors. Callers of this function must handle the error.
```

### Dealing with errors

As a more practical example, a theoretical interface for accessing a value from a map could be the following:

```derg
struct KeyNotPresent

struct Map[Key, Value]
{
    val data // Details omitted.
}

fun Map.get(key: Key): KeyNotPresent -> Value
{
    if !data.contains(key)
        raise KeyNotPresent
    return data.at(key)
}
```

This interface could on the caller side be used in the following manner:

```derg
val value = map.get("key") : "default-value" // Or any other error-handling operator.
```

Any valid [error-handling operator](operators.md#error-operators) could be utilized here, providing a succinct way to
handle arbitrary errors. Note that this mechanism does not rely on exceptions, but instead provides a more clean way of
representing errors as a sum type. A function may return either a value, an error, or neither, but never both.
