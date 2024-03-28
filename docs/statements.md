# Statements

## Control flow

### Branch

The branching statement allows the execution flow of the program to be controlled via a predicate. Whenever the
condition evaluates to `true`, the success branch is followed. Otherwise, the failure branch is. The execution flow will
only take one of the two branches, never both.

Branches are declared using the `if` keyword, and require an expression which must evaluate to a boolean value. The
success branch is required, and may contain any number of instructions in it. The failure branch is optional, and is
declared using the `else` keyword. Note that the branches should never contain zero statements, although the language
does not restrict the number. When zero or more than one instruction is present within any branch, the instructions must
be enclosed in brackets (`{´ and `}`).

Examples:

```derg
// Simplest form, if the condition evaluates to true, the statement is executed.
if condition
    statement

// A failure branch which will be executed if the condition evaluates to false.
if condition
    success
else
    failure

// Multiple conditionals chained together, exactly one branch will be executed.
if condition_1
    statement_1
else if condition_2
    statement_2
else
    statement_3

// Multiple statements within a single branch.
if condition
{
    statement_1
    statement_2
}
```

### Evaluate

The evaluate statement allows a piece of code to be executed for its side effects. When evaluating an expression, both
the value type and error type of the expression must be absent. This has the consequence that expressions which evaluate
to a value or an error, cannot be used as statements - any expression which produces a value, must be consumed. If the
expression produces an error, the error must be handled.

Some expressions naturally do not produce any value, such as a function call to the function which has no return value
or error.

Examples:

```
fun do_work() { /* Implementation omitted */ }
fun make_it() -> Data { /* Implementation omitted */ }

do_work() // Legal, function returns no value, nor does it raise any error.
make_it() // Not legal, expression has a value type associated with it.
9001      // Not legal, expression has a value type associated with it. 
```
