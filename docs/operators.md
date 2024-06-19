# Operators

## Error operators

### Catch

The catch operator takes an expression which has an error type, and converts it into another expression which has the
same value type as the original expression. This operator allows the developer to convert the error condition into a
value which can be used as a default value.

If the expression on the left-hand side is not an error, the expression evaluates to the left-hand side value.
Otherwise, the error will be replaced with the value on the right-hand side. The error is implicitly available under the
name `it`.

Example:

```derg
val value = add(1, 2) : 0
val value = add(1, 2) : compute_default_value(it)
```

### Catch-Raise

Similarly to the catch operator, the capture-raise operator transforms the error into a value which is raised from the
current function. This operator is considered a version of early returning, returning a failure error from the function.

If the expression on the left-hand side is not an error, the expression evaluates to the left-hand side value.
Otherwise, the function will raise an error based on the right-hand side expression. The error is implicitly available
under the name `it`.

Example:

```derg
val value = add(1, 2) !: 0
val value = add(1, 2) !: apply_error_context(it)
```

### Catch-Return

Similarly to the catch operator, the capture-return operator transforms the error into a value which is returned from
the current function. This operator is considered a version of early returning, returning a success value from the
function.

If the expression on the left-hand side is not an error, the expression evaluates to the left-hand side value.
Otherwise, the function will return a value based on the right-hand side expression. The error is implicitly available
under the name `it`.

Example:

```derg
val value = add(1, 2) ?: 0
val value = add(1, 2) : compute_fallback_value(it)
```

## Logical operators

### Negate

Certain types may be inverted through the negate operator. What constitutes negation depends on the type which should be
negated; for real numbers, the negate operator inverts the sign of the number from positive to negative, or vice versa.

Example:

```derg
val value = -1
```
