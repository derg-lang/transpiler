# The type solver

As programs scale, type information become more and more useful - not only as documentation for the developers working
with the code, but as a contract ensuring that the developer do not break any expectations unknowingly. In Derg, type
information is handled in a unique way - types are first-class citizens. This has major consequences on the working of
how type information is "solved".

## Order of operations

The type-checker passes over all code in a single step, recording all information it needs before starting type
resolution. However, code can be written in such a way that it must be type-checked in a specific order. Take the
following code as an example:

```derg
val foo: type = __builtin_int32
val bar: foo = 0
```

This program cannot be type-checked in a single phase, as the type stored in `foo` is not known before it is evaluated.
The evaluation cannot happen before the type-checker has passed over the code, as we do not yet know if the code is
well-typed. This seems like a chicken-and-the-egg problem, where we need to type-check the code to evaluate it, but we
cannot type-check it before we have evaluated it.

The solution lies in realizing that parts of the program can be type-checked independently of the rest, and we can
perform type-checking only on these parts at first. We can then perform all evaluations which have now been unlocked by
the type-checking, then perform more type-checking based on those evaluations. This process can repeat until all code
has either been type-checked, or we reach some code which cannot be evaluated due to a circular dependency, in which
case we reject the program as incorrectly typed.

We need to track all code which should execute during the type-checking phase. All code in this category falls under a
certain umbrella term - _compile-time code execution_. This code must be executed intermittently with the type-checker,
ensuring a back-and-forth process.

To wet our appetite on what this means in process, assume a built-in type `Int32` exists. Given the following code:

```derg
val foo: Int32 = 0
```

Here, we cannot fully type-check the program in one cycle. We can start building up our constraints, and the order in
which we must resolve type information:

- Mark `foo` as depending on `Int32` being fully type-checked.
- Generate a constraint stating that `Int32` must have kind `type`.
- Type-check `Int32`, resolving the potential name ambiguities to the correct symbol.
- Type-check `foo`, now we are able to evaluate `Int32` to indicate which type we are interested in.

This does imply that during this process, we also perform lowering of the code into something which we can execute on
our interpreter.

Let us examine a more complex example to make this process more clear:

```derg
val foo = magic() // Not sure if `foo` is a type or value yet, since we cannot evaluate what `magic()` is right now.

fun bar(input: foo) {} // Require us to know what `foo` is before we can type-check.
```

Once we start working on this code, we run into the issue of not being able to determine the type of either `foo` or
`bar` without first resolving the value of `Int32`. The process of type-checking looks like the following:

- Mark `bar` as depending on `foo`.
- Generate a constraint stating that `foo` must have kind `type`.
- Type-check `foo`, determining that it is indeed a `type` and references symbol `#1`.
- Type-check `bar`, evaluating `#1` to a concrete type, assigning this type to `bar`.










```derg
struct Array[Type: type, size: Int32]
{
    // Implementation omitted
}

fun concatenate[Type: type, lhsSize: Int32, rhsSize: Int32](
    lhs: Array[Type, lhsSize],
    rhs: Array[Type, rhsSize],
) -> Array[Type, lhsSize + rhsType]
{
    // Implementation omitted
}

val foo: Array[Int32, 2] = magic()
val bar: Array[Int32, 3] = magic()
val baz = concatenate(foo, bar)
```
