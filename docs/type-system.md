# Type System

The type system of a programming language is perhaps one of the strongest advantages for writing large pieces of
software. With a strong type system, a developer can rest assured that the compiler can detect and report a large number
of erroneous programs, without the developer needing to run the program in the first place. The compiler can deduce that
a function cannot be called with some values, that certain values are unused, errors are not handled, and much more.

In Derg, the type system allows the developer to express their intent clearly and concisely. All values are associated
with a type, which determines how the value can and cannot be used. The shape of the data, visibility, mutability,
parameter passing rules, and so on influences the possible ways values may be passed around.

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

## Error handling

Traditional programming languages are often based on a form of exception mechanism, return value, or some form
of `Result` type. These approaches have advantages and disadvantages, typically in some way of being too intrusive and
unergonomic, easy-to-forget to handle, or some other variant thereof. Derg seeks to use a `Result` approach, but rather
than repeating the mistakes of other languages with this approach, the error path will be a first-class citizen.

Typically, functions can fail if one or more invariant is violated, and the type system is unable to capture the error
to make it impossible. This can for example be the case when the developer wants to access a value stored in a map of
some sort, and want to access a non-existing key. As we cannot easily or at all detect such errors, we need an
alternative way to represent this error. Other languages opt in for a `null`, `undef`, or `Option` variant, but Derg can
express this violation in a far more powerful feature: the error track.

### Error track

All functions in Derg may be declared with a return value, and error value, neither, or both. A function which does not
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
