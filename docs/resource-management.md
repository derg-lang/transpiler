# Resource management

Derg does not rely on garbage collection for resource cleanup. Instead, it uses a deterministic approach to resource
management, similar to RAII (Resource Acquisition Is Initialization) in C++. Instead, all resources, including memory,
are treated equally and managed by the objects that hold them. Once a resource is assigned to an object, it can be used
throughout the program. When the object goes out of scope, the resource is automatically released.

This method relieves developers from manually cleaning up memory, file handles, sockets, or any other limited resource.
The compiler automatically inserts destructors whenever necessary, ensuring everything is eventually freed.

## Lifetimes

Every piece of information in a program has a specific lifetime, determined by the scope in which it appears and how it
is passed between different scopes. Lifetimes begin when information is created and end when it is destroyed, regardless
of how it is passed around.

The lifetime of an instance can only be extended by transferring ownership to other parts of the program. Transferring
ownership means moving the resource, making the original variable invalid; the original variable no longer owns any
information, therefor it cannot be used. For example:

```derg
fun my_function()
{
    val one = MyResource()
    val two = one // Ownership of `one` is transferred to `two`.
    // `one` can no longer be accessed here.
}
```

In Derg, information is created through *constructors*, which allocate resources from the machine running the program.
This information is bound to a specific scope, which determines its lifetime. When the information leaves the scope, it
is released through *destructors*, which clean up used resources.

Both constructors and destructors can be customized for different types, giving developers full control over resource
creation and destruction.

### Constructors

Constructors are responsible for creating valid instances of a certain type, ensuring all fields are fully initialized.
If a constructor cannot create a valid instance, it must raise an error.

Developers can declare new constructors for any type as follows:

```derg
struct MyType
{
    val field_a: Int32
    val field_b: Int32 = 42
}

ctor MyType.my_constructor(parameter: Int32)
{
    field_a = parameter
}

val instance = MyType.my_constructor(1337)
```

A constructor that raises an error can be declared like this:

```derg
ctor MyType.fallible_constructor(parameter: Int32): MyError
{
    if parameter <= 0
        raise MyError
    
    field_a = parameter
}
```

*All* fields of the object must be initialized by the constructor. Any field which has a default value, does not have to
be explicitly initialized, however. A field which has not yet been initialized, cannot be used when executing code in
the constructor. If a field is not initialized and has no default value, a compile-time error is raised. Once a value
has been assigned to the field, it may be used as any other variable for expression evaluation, so long as the field
holds a non-moved value at the end.

Note that all fields must be initialized in the order they appear in the struct itself. A field cannot refer to the
value of a field declared later within the constructor; for example, `field_a` cannot be assigned the value of
`field_b`, but `field_b` can be assigned the value of `field_a`.

A default constructor will be defined for all defined structs. This constructor will initialize all fields in the order
in which they appear in the struct, and allows the developer to override any of the default values. Given the example
above, the default constructor could be used in the following manner:

```derg
val instance_a = MyType(field_a = 7, field_b = 10) // Assigning both `field_a` and `field_b`.
val instance_b = MyType(field_a = 1337)            // Only assigning `field_a`, `field_b` uses the default value.
```

Constructors are first-class citizens and may be used as parameters. Constructors have a function type, which takes the
shape `($parameter-list): $error -> move $type-name`. A developer may as such pass constructor functions into other
functions when needed. Constructors may be passes as parameters in the following manner:

```derg
fun do_work(constructor: () -> move MyType)
{
    val resource = constructor()
} 
```

### Destructors

Destructors perform the opposite operation of constructors, defining how a resource should be destroyed once it goes out
of scope.

Every type must have a destructor in one shape or another, as anything created *must* be destructible. Typically, every
type can rely on the default destructor, which destroys all fields in the struct in the appropriate order. However, some
specialized types may want to customize the destructors, allowing the developer to control what happens when the object
is destroyed. Custom destructors may be declared in the following manner:

```derg
dtor MyType.my_destructor(parameter: Int32)
{
    // Do something.
}
```

Within a destructor, *all* fields of the struct must be destroyed, in the opposite order in which they were initialized.
Any field not explicitly destroyed within a destructor, will be implicitly destroyed, ensuring that the developer does
not forget anything. Note that if the developer want to destroy any field in a struct manually, the developer must also
explicitly destroy all the other fields, too.

Note that a destructor is not permitted to raise errors at all. Cleaning up resources is an operation which must succeed
in all circumstances.

Destructors are first-class citizens and may be used as parameters. Destructors have a function type, which takes the
shape `move $type-name.(): $error`. A developer may as such pass destructor functions into other functions, and use them
as if they were normal destructors. For example, a destructor may be used in the following manner:

```derg
fun do_work(destructor: move MyType.())
{
    val resource = MyType()
    resource.destructor() // The resource is destroyed here.
} // `resource` is already destroyed, no destroyer is injected here.
```

## Scopes

In programming languages, a scope is a context in which symbols (such as variables, functions, and types) are defined
and can be accessed. It determines the visibility and lifetime of these symbols within different parts of a program.
Scopes help organize code, manage lifetimes of resources, and prevent naming conflicts.

In Derg, scopes function similarly to other programming languages. They define the visibility boundaries of variables
and resources and control their lifetimes. When an object or variable is created within a scope, it is only accessible
within that scope and any nested scopes provided the [visiblity rules](type-system.md#visibility-modifiers) are obeyed.

There are two main types of scopes:

- **Global scope**: The global scope is the outermost scope that covers the entire program. All symbols within this
  scope are accessible from any other part of the program. Symbols are automatically located in the global scope when
  they are declared at the top-level of any [segment](program-structure.md#segments).
- **Local scope**: A scope which is limited to a specific block of code, such as a struct, function body, conditional
  blocks, loops, and so on. Symbols defined within this scope, can only be accessed by name in the same or a nested
  scope.

Scopes can be nested within other scopes. For example, a local scope inside a function may contain further nested local
scopes within loops or conditional blocks. Inner scopes can access names from their outer scopes but not vice versa.

Here is an example illustrating the scoping visibility rules:

```derg
export fun example_function() // `example_function` is in the global scope. It is reachable everywhere by name.
{
    val outer_scope_var = 10 // `outer_scope_var` is in the function's local scope.

    if outer_scope_var > 5
    {
        val inner_scope_var = 20 // `inner_scope_var` is in the nested local scope (inside the if-statement)
        // Can access both `inner_scope_var` and `outer_scope_var` here
    }

    // Can only access `outer_scope_var` here; `inner_scope_var` is out of scope
}
```

Scopes are closely tied to the concept of lifetimes in Derg. The lifetime of a resource is determined by the scope in
which it is defined. When the scope ends, any resources created within it are automatically released. When an object
goes out of scope, its destructor is called automatically to release any resources it holds. This happens at every point
where the control flow leaves the scope, such as at the end of a function or when a return statement is executed.

```derg
fun my_function()
{
    val resource = MyResource() // Resource is created and owned by this scope.

    if some_condition
        return // Resource's destructor is called here.

    // Resource's destructor is called here if `some_condition` is false.
}
```

### Ownership

Every piece of information in a source program has exactly one owner, even for temporary r-values. Typically, the owner
of a resource will be the variable or field which stores the resource, but a function may also own resources according
to the [parameter passing rules](type-system.md#value-passing).

A resource is always bound to an owner when created and lives until the owner goes out of scope, at which point the
compiler injects a destructor call to release the resource:

```derg
fun my_function()
{
    val resource = MyResource() // `resource` now owns the resource and will manage it automatically.
} // `resource` is now out of scope, and the destructor for `MyResource` will be injected here.
```

A destructor call is injected at every branch where control flow leaves the function body. This ensures that the
resources are always cleaned up automatically, no matter what function the developer writes.

```derg
fun my_function(parameter: Bool)
{
    val resource = MyResource()
    if parameter
        return // `resource` is destroyed here.
} // `resource` is also destroyed here, since there is no way from the other branch to here.
```

Passing the ownership of resources is possible, and can be performed in various manners:

```derg
fun producer() -> move MyResource
{
    val resource = MyResource()
    return resource // Ownership is transferred to the caller.
}

fun consumer(move resource: Resource) // Ownership is transferred to the function.
{
} // Destructor for `resource` runs here as it goes out of scope.
```

A destructor will not be inserted in situations where the ownership is transferred, however.

When a value is stored within the field of a struct, the ownership model is somewhat more complex, but the same
principles still hold. The resource stored in the field will only be destroyed once the struct instance goes out of
scope. See [destructors](#destructors) for further information.

### Borrowing

Merely owning information is not enough to write a program of sufficient complexity. There are many situations where a
developer needs data to be shared among different tasks, and copying data around yields insufficient performance for
modern applications. Instead, data may be shared by *borrowing* information, allowing information to be accessed but not
destroyed.

Borrowing does come with a few considerations, however. If the information which is borrowed, is destroyed while
borrowed, the program is ill-formed and can result in disastrous outcomes. There are multiple security vulnerabilities
available, which are caused by reading or writing to deleted data, which is a security issue Derg seeks to rectify. The
core issue is that a developer cannot be permitted to write a program, such that anything refers to data which has gone
out of scope.

To accomplish such a goal, Derg ensures that there cannot ever be a reference to anything, which may be destroyed. This
phrasing does demand a few examples to illustrate the point:

```derg
fun returning_local_variable() -> borrow MyResource
{
    val resource = MyResource()

    return resource // Compiler error, `resource` is no longer valid once the function returns, lifetime is too short.
}

fun accessing_moved_variable()
{
    val old = MyResource()
    val new = old
    
    println(old.data) // Compiler error, `old` does not hold any data here, `new` owns the data instead.
}

fun accessing_destroyed_variable()
{
    val resource = MyResource()
    resource.custom_destructor()
    
    println(resource.data) // Compiler error, `resource` has been destroyed.
}

fun destroying_borrowed_variable(borrow resource: MyResource)
{
    resource.custom_destructor() // Compiler error, cannot destroy non-owned data.
}
```

This safety runs much deeper than just examining whether a value is borrowed or not. In Derg, if any information is
referenced, it is not possible to destroy at all:

```derg
fun destroying_borrowed_data()
{
    val owned_resource = MyResource()
    ref borrowed_resource = owned_resource
    
    owned_resource.custom_destructor() // Compiler error, cannot destroy instance while borrowed by `borrow_resource`.
}

fun mutating_list_while_borrowed_element()
{
    val list = list_of(MyResource(), MyResource())
    ref borrowed = list.first()
    
    list.add(MyResource()) // Compiler error, `list.elements` is potentially destroyed while `borrowed` references an
                           // element among the elements.
}
```

### Memory tagging

// TODO: Heavily in development here!

In order to determine whether information is potentially destroyed when referenced, Derg ensures that every symbol
holding any information is given a unique tag.

```derg
struct List[Type]
{
    var size: Int32
    var data: __builtin_ptr[Type]
}

ctor List()
{
    size = 0
    data = __builtin_allocate_heap_memory[Type](16)
}

fun myt List.add(move element: Type)
{
    size += 1
    if size == data.capacity
    {
        // Perform reallocation here.
    }
    data.set(size, element)
}

struct MyData
{
    val resources: List[MyResource]
}
```

### Concurrency

Concurrency is critically important in high-performance modern applications, but concurrency can cause major issues in
regarding memory safety. In traditional languages, concurrency-related bugs are commonly vexing and difficult to fix. In
Derg, concurrency should be simple to understand and developers should not worry about the safety of the application.

When taking concurrency into view, it is no longer sufficient to simply consider whether a reference to memory is still
valid or not; multiple heads of execution might add elements to the same list at the same time, resulting in
inconsistencies to the internal state of the list. For example, two heads of execution might at the same time update a
list's internal size tracker. Later in the program, an invalid entry might be accessed, resulting in ill-defined
behavior.

To prevent concurrency-related issues, Derg enforces structured concurrency and prevents multiple heads of execution
from mutating the same memory. When spawning additional tasks, any information might either be mutated by *a single*
head of execution, or it can be read by *any number* of heads of execution. Due to structured concurrency, all tasks
must complete before the surrounding head of execution is permitted to make any progress.

Multiple heads of execution can be spawned using a spawn function:

```derg
val data = generate_data()

// Spawning three sub-processes which perform read-only operations on `data`.
run
{
    operation_one(data)
    operation_two(data)
    operation_three(data)
}
// All child processes are complete at this point, Derg guarantees that all heads of execution are merged before control
// flow is returned to the original caller.
```

All variables passed into the spawn pool must be read-only. Mutation of shared memory is not permitted:

```derg
val data = generate_data()

run
{
    mutate_data(data) // Compiler error, cannot mutate data in another head of execution. 
}
```

Rather than mutating data, sub-processes should instead return their output, which can be concatenated and processed my
the main process instead.
