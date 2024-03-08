# Literals

Literals are functions which take in a single parameter of a builtin data type, converting the value into some other
type. This output type may be any builtin or user-defined type. Literals are only permitted to operate on raw literals,
and cannot be invoked in any other way.

Normally, all raw integral numbers are converted to a signed and sized integer. The default size is language-defined (
i.e. 64 bits wide), although the developer can use any other bit width by specifying the literal explicitly. Similarly,
raw decimal numbers are by default converted into a floating-point number, whose width is language-defined. Raw string
literals are given the same treatment.

## Raw literals

Raw literals are the constants a developer embeds into their source code. In isolation, it is impossible to determine
which data type raw literals are - there must always be some hint suggesting which type they should be encoded as. This
hint is where literals come into play.

Raw literals come in three different flavors: integers, decimals, and strings.

- **Integers**: Integers are non-negative integral numbers which have an arbitrary number of digits in them.
- **Decimals**: Decimals are non-negative decimal numbers which have an arbitrary number of digits in them.
- **Strings**: Strings are arbitrarily long sequences of Unicode characters. Raw string literals may not include the `"`
  character, however, unless it is escaped (`\"`). The backwards slash must be escaped (`\\`) as well, if it should be
  included in the raw string literal.

Should the numeric raw literal be outside the permitted range of values to fit in the type expected by the literal (i.e.
overflow), then compilation fails. Note that negative raw numeric literals are not permitted by definition; in order to
acquire a negative number, the [negate operator](operators.md#negate) must be applied to the output of the literal
function.

All raw literals are converted into a specific type through exactly one literal function. The developer may either
specify which literal to use to convert the raw literal into a concrete type, or rely on the default literal for the raw
literal's flavor.

## Defining literals

Literals are declared in a manner similar to a function:

```derg
lit $literal_name($parameter_name: $builtin_type) -> $type_name
```

All literals must have exactly one parameter, which must be of a builtin type. Literals must return exactly a single
value and are not permitted to have errors. Any error which is raised within the function body are caught at
compile-time, preventing illegal raw literals from being embedded into the source code.

User-defined literals may be defined in the following manner:

```derg
lit mps(value: Float64) -> Speed
{
    return Speed(value)
}

lit kmph(value: Float64) -> Speed
{
    return Speed(0.27778 * value)
}

lit email(value: String) -> Email
{
    return parse_email(value)!
}
```

These literals may be used in the following manner:

```derg
val walk_speed = 1.3mps
val run_speed = 10.7kmph

val address = "my.email@domain.com"email
```

Note that the literal parameter must be of a builtin type. User-defined input parameters are forbidden, as the raw
literal cannot be automatically converted into user-defined types.
