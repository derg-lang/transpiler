# Data structures

A program cannot hold state without a data type capable of storing information. Data structures are symbols which take
an arbitrary amount of space in memory, to store some information. Such information may represent some combination of
primitive units such as integers, floating points, boolean values, and/or other data structures.

Note that there is no difference between a primitive data structure such as an integer, and a user-defined data
structure. Typically, primitive data structures will not contain any extra functionality, only the semantics of the type
itself. User-defined data structures will often contain fields and methods, which may make them different, but there is
no conceptual difference.

Here is a few quick examples of how a data structure may be defined in code:

```derg
export struct DivideByZeroError

private struct Circle(val radius: Float32)
{
    fun area(borrow this): Float32
    {
        return PI * radius * radius
    }
}

public struct List[Type](initial_capacity: Int64 = 8)
{
    var size = 0
    var capacity = initial_capacity
    var elements = allocate_heap_memory[Type](initial_capacity)
}

public Wrapper[Data](val data: Data) 
{
    ctor { print("Acquired resource $data") }
    dtor { print("Released resource $data") }
}
```

## Fields

Data structures are permitted to contain fields, which is a binding to an inner data structure. Fields can combine
multiple data structures into one more complex data structure, forming a composite type. More complex data types can be
used to express more specific semantics, and to make parameter passing simpler.

Fields are permitted to have a visibility modifier, which ensures that encapsulation can be enforced. Fields may be
accessed or modified only if the field is visible from the current scope. By utilizing fields, it is possible to
construct abstractions, which can encapsulate and hide state.

When a user declares a field, the field must be given a mutability modifier. This modifier determines whether the field
can be bound to another instance, or whether it will remain unbindable throughout the lifetime of the data structure
instance. Any field marked as `var` can be rebound to some other value, whereas any field marked as `val` cannot be
rebound. Developers are highly encouraged to mark every field as `val`, and only resort to `var` when this is the only
option.

Once an instance of the data structure is instantiated, every field must have been given a value for the instance to be
valid. In the event that the data structure has multiple constructors, every field must be initialized exactly once in
every single constructor.

The field type may be inferred from an initialization expression if the field is immediately assigned a value. If the
assignment of the field is delayed to a secondary constructor, an expression may be omitted, but the developer must in
this case explicitly state what the type is.

The syntax for declaring a field is as follows:

```
$field := $visibility $mutability $identifier $[: $type]? $[= $expression]?
```

Fields may be declared in the following manners (non-exhaustive list of examples):

```derg
public val field_a = 0
public val field_b: MyType
private var field_c: MyType = some_factory()
```

## Methods

Data structures can be associated with functions as well. Methods are in similar to functions, although they have
different scoping and invoking rules. While functions can be called in a freestanding form, i.e. `function()`, methods
are always associated with an instance:

```derg
val my_instance = MyType()

my_instance.method()
```

In this example, the method can only be invoked when there is one or more instances of the type available. Invoking the
method on one instance may have a different effect from invoking the method on another instance. For example,
invoking `list.append(element)` will mutate `list`, but another list will remain unchanged.

The key difference between methods and functions, is that methods are bound to a type, thus having access to additional
state which would normally be invisible to functions. The additional state is accessible through the `this` parameter,
which is always an instance of the type the method is bound to. This parameter may be passed in with any regular
transfer semantics, depending on the specific needs of the programmer. The `this` parameter may be left out entirely, in
which case the method behaves like a static function, but under the namespace of the data structure.

Methods are normally not permitted to mutate the `this` parameter in any way. Any method which needs to mutate it, must
be declared `mut`. A method which is declared `mut` may only be invoked from another `mut` method on the same instance,
and/or on an instance whose type has been declared `mut`.

The syntax for declaring a method is as follows:

```
$method := $visibility fun $identifier ($parameter-list) $[mut]? $[: $value-type]? $[-> $error-type]? { $statement-list }
```

Methods may be declared in the following manners (non-exhaustive list of examples):

```derg
export fun get_size(): Int32
{
    return 42
}

export fun close(this)
{
    println("Closed file")
}

private fun do_work() mut
{
    my_field += 42
}

public fun append(borrow that: String) mut
{
    contents += that
    return this
}
```

A user may invoke methods on an instance in the following manner:

```derg
val returned = instance.get_something()

instance.do_work()
instance.do_more(returned)
```

## Data structure lifecycle

All instances of any data structure exists only for a limited amount of time. The lifetime of instances is determined by
their scope, and whether they are moved from one scope to another. The lifetime begins at the moment an instance is
created, and ends when the scope owning the instance ends.

The creation of instances happens via a constructor, which is a factory function producing an instance of a specific
data structure. Destructors perform the opposite operation, they are a function which takes ownership over the instance,
and does not give the ownership to anything else.

When an object is created, every field are initialized in order of declaration. Whenever the object goes out of scope,
every field is destructed in the reverse order.

### Constructors

Data structures cannot be summoned into existence out of nowhere. One way or another, the methods for instantiating a
new instance of the data structure must be defined. Constructors can be seen as factory functions, where they have the
sole purpose of ensuring the instance they attempt to produce is either fully constructed, or not constructed at all.
The primary task of the constructor is to ensure that all fields are initialized.

Constructors always return the same type as the data structure they are declared within, and in some cases they are
permitted to raise errors. Raising errors is useful to communicate that the instance could not be constructed, which the
client code must respect and handle. Note that in the event the instance could not be constructed, any fields which were
initialized will be destructed in the opposite order in which they were initialized.

**Primary constructor**

The primary constructor is the core way of defining how an instance of the data structure can be constructed. It can
take in any number of parameters and field declarations in arbitrary order. Primary constructors initialize fields in
declaration order and cannot raise errors. Note that unlike in the data structure body, any field declared in the
primary constructor must specify which type it is.

The visibility of the primary constructor may be omitted, in which case it takes the same visibility as the data
structure. Otherwise, the primary constructor can be given an explicit visibility, which may be wider than that of the
data structure itself. If the visibility is wider than the structure, it would be possible to instantiate an object of
the data structure, but it would not be possible to reference its type.

The primary constructor must appear immediately after the data structure itself has been declared, before the body of
the structure. If no primary constructor is provided, and if all fields have default values assigned to them, a default
primary constructor taking no parameters is generated by the compiler. This constructor will create a new instance with
the default values.

The syntax for declaring the primary constructor is as follows:

```derg
$constructor-primary := $[visibility]? ( $field-and-parameter-list )
```

A non-exhaustive collection of primary constructor examples is as follows:

```derg
(parameter: Int32)
(parameter: Int32 = 42, var field: Bool = false)
public (val field_a: Int32, val field_b: SomeType)
private (val field_a = 0, parameter_a: SomeType, var field_b: AnotherType)
```

A user may invoke the constructor as if the data structure was a function, in the following manner:

```derg
struct MyType(val a: Bool, val b: Bool = false)

val instance_a = MyType(true)
val instance_b = MyType(true, b = true)
val instance_b = MyType(b = false, a = false)
```

In some cases, a user may want to execute arbitrarily code in the primary constructor. This is possible by defining an
initialization block using the `ctor` keyword. This initialization block is executed once all attributes of the instance
are initialized.

The initialization block may be declared in the following manner:

```derg
ctor
{
    // Abritrary code.
}
```

**Secondary constructors**

Secondary constructors enable a developer to specify alternative means to construct an instance, using more concise and
clearer semantics.

// TODO: Write me.

```derg
export ctor from_spherical(theta: Float64, phi: Float64, radius: Float64): Point3D
{
    return Point3D(x = radius * cos(theta) * cos(phi), y = radius * sin(theta) * cos(phi), z = radius * sin(phi))
}
```

### Destructors

Destructors are the opposite of constructors - these are functions which takes an object apart and deconstructs all its
components as necessary. Destroying an object ensures that all resources it used are released, and that its contents can
never be accessed again. This is a crucial part of resource management, as resources will automatically be cleaned up
once they are no longer used.

Destructors will always be invoked either automatically by the compiler once an object exists its scope, unless manually
invoked by a developer. The compiler will only invoke the primary destructor if it exists; in the case where no such
destructor has been defined, the programmer is required to invoke exactly one of the alternative destructors. Note that
the programmer cannot invoke the primary destructor manually, only secondary constructors may be invoked.

**Primary destructor**

The primary destructor has no name, takes no parameters, it returns nothing, and is not permitted to fail for any
reason. It cannot be invoked by the developer, and every call to it will be automatically inserted by the compiler. This
destructor will be invoked whenever any instance is leaving a scope after its definite last use, which also includes at
the end of every secondary destructor.

Usually, the primary destructor is generated automatically by the compiler. If the developer declares any destructor, be
it the primary constructor or any secondary ones, no primary destructor can be generated. Additionally, if any of the
fields within the data structure lacks a primary destructor, the developer is required to manually declare the primary
destructor or any alternative ones.

The syntax for declaring a primary constructor is as follows:

```derg
dtor
{
    // Arbitrary code.
}
```

If any of the attributes lack a primary destructor, the compiler is unable to generate a default destructor. In these
cases, the developer must either define a custom primary destructor, or define at least one secondary constructor where
the attribute is destroyed.

**Alternative destructors**

A developer may sometimes want to guarantee that some functionality is invoked before an object is destroyed. This can
be achieved by defining an alternative destructor, which is named and may take any number of parameters. These
destructors may return values, or raise errors if necessary. Regardless of how the destructor is exited, the object will
be fully cleaned up.

Once a developer defines a custom destructor, the data structure will have no more default destructor. This means that
an object of this data type cannot be automatically destroyed. As a consequence, the developer must manually invoke
exactly one of the custom destructors must be invoked, otherwise the program is considered malformed. Note that the
object does not have to be destroyed, provided it is moved out of the destructor.

Note that the visibility of a destructor may make it impossible for certain consumers to destroy an object. This allows
developers to enforce that a resource borrowed elsewhere is returned for destruction.

The syntax for declaring a destructor is the same as for declaring a method. However, for a method to be interpreted as
a destructor, it must take the `this` parameter by `move`.

A non-exhaustive collection of destructor examples is as follows:

```derg
export fun release(move this)
{
    log.info("Object has been released")
}

public fun commit(move mut this) -> TransactionError
{
    database.commit() !: DatabaseCommitFailed(this) 
}
```
