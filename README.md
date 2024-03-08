# Derg Transpiler

Transpiler for the Derg Programming Language

# Compiler structures

## Abstract Syntax Tree (AST)

All source code is represented as AST. This tree does not describe the semantics
of the program, nor how the program flows, behaves, is linked together or
anything of that sort - it is merely a direct representation of the source code
itself.

The purpose of the AST is to capture the *shape* of the program rather than the
*intent*. The AST will later be evaluated and restructured to capture programmer
intentions at later phases in the pipeline.

## High Intermediate Representation (HIR)

This stage captures the core structure of the program before any type resolution
takes place. The core structure and architecture of the source code is captured
at this phase, including all relevant dependencies and how all modules are
intended to be tied together.

All optional and missing parameters in the AST will be resolved to their default
values at this stage, ensuring the entire source code is represented in the most
complete form possible. This phase of the compilation process initiates data
validation, ensuring invalid programs are rejected as soon as possible.

## Typed High Intermediate Representation (THIR)

After the initial source code has been captured as AST, it can be converted into
a higher-representation of the executable code. The intention of this structure
is to capture the relations of the code, resolving all references and to ensure
that the program is type-safe.
