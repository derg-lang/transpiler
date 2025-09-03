# Type checking

Catching issues in software during the compilation phase moves many categories of errors into compile-time rather than
having the software fail while running. While developers may find it harder to write software with a strong type system,
the benefits provided in terms of maintainability, are hard to argue against.

The type checker ensures that a number of qualities of the software are maintained - for example, the type system
ensures that a value must be what the developer expects it to be at runtime, that values not marked as mutable are not
changed unexpectedly, that released or otherwise inaccessible memory is not accessed, and much more.

## Phases

The type checker is split into multiple phases, each covering different parts of the type resolution.

The first phase is to capture the names of all identifiers which are part of the program. These identifiers are used to
construct a large potential call graph, indicating how all functions and data related to each other. The second phase
attempts to solve the graph, in such a way that all names are resolved to exactly one viable candidate.

