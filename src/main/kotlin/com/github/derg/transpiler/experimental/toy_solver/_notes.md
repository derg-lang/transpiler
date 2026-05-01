1. Types as First-Class
    - Types and compile-time values coexist in one expression language.
    - Generic parameters may be types or compile-time-evaluable values.
    - A simple kind system separates type-level and value-level parameters.

2. Generics With Constraints
    - Generic parameters require trait/interface bounds.
    - Functions, methods, and records can all be generic.
    - Type arguments may be explicit, inferred, or computed via compile-time expressions.

3. Compile-Time Execution
    - Any expression used as a type argument must evaluate at compile time.
    - Compile-time functions must be pure and deterministic.
    - Compile-time results (including types) can be stored in variables.

4. Union Types and Flow Typing
    - Union types (A | B) are first-class.
    - if x is T narrows the type in the corresponding branch.
    - Merging branches computes the least upper bound.

5. Records and Interfaces
    - Records (structs/classes) are nominal types with structural field access.
    - Interfaces define required methods and (optionally) fields.
    - Records may implement interfaces, enabling generic constraints.

6. Overloading and Call Resolution
    - Overloaded functions/methods are resolved via:
        - Candidate filtering by arity/kinds
        - Unification with argument types
        - Constraint satisfaction
        - Most-specific selection

7. Constraint-Based Type Inference
    - HM-style unification extended with:
    - union types
    - trait/interface constraints
    - record field access
    - overloaded call resolution
    - compile-time evaluation
