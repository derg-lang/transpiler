# Program Structure

## Packages

A package is a single standalone unit of software. The package is a single folder containing additional folders of
source code. Each sub-folder in the package corresponds to a new module, and every source code file at the root level of
the package represents the default module.

Given an empty project, all packages may be declared in the following manner:

```
root-directory:
  src:
    bin:
      application-a.derg
      application-b.derg
    lib:
      package-a
      package-b
    tests:
      package-a
      package-b
```

The test folder is optional but highly encouraged. All test code is part of the package, but will only be relevant when
compiling the source code when running tests. The production version of the source code will not contain the test code.

Packages are permitted to depend on each other, although circular dependencies are not permitted. In order to compile a
package, all dependencies of that package must have been compiled. Dependencies between packages can be implicitly
resolved based on the modules they contain; by examining the modules imported, we can infer the order in which all
packages must be compiled.

// TODO: Write about external packages, how can third-party code be included in the application/library?

## Modules

Modules are among the main methods for organizing source code in a structured and coherent manner. Modules can contain
any number of segments, each of which contribute to the module itself. Any symbol which is defined in any of the
segments, will be made available to the module as a whole, provided
the [visibility rules](type-system.md#visibility-modifiers) permit so.

Each segment is represented as a regular source code file. While the name of the module is meaningful, the name of each
segment file is not. The name of segment files may be used to hint to the contents of said file. Note that a module does
not need to contain any source code files, in which case the module does nothing and holds no content.

Modules may contain additional submodules. Submodules can be declared by simply creating a sub-folder within the module
folder. Files placed in the submodules does not contribute to the parent module; they are only applicable to the nearest
defined module. This means that anything declared in a submodule, does not exist in the parent module - these modules
are fully unrelated to each other.

The folder structure of modules are declared in the following manner:

```
package-directory:
  module-a:
    submodule-1
    submodule-2
  module-b:
  module-c:
```

In order to access symbols from a different module, the developer must explicitly import the symbols into the current
scope. This could be done in the following manner:

```derg
use module           // Imports all symbols from the module into the current scope.
use module.submodule // Imports all symbols from the submodule into the current scope.
use module my_symbol // Imports only `my_symbol` from the module into the current scope.
```

If multiple symbols with the same name are defined within the module (i.e. overloaded functions), all those symbols are
imported. In the case of name collisions, symbols may be aliased to avoid issues. Aliasing is possible in the following
manner:

```derg
use module my_symbol as alias // Imports `my_symbol` under the name `alias`.
```

Like packages, modules are not permitted to have circular dependencies. A module cannot be compiled before all other
modules it depends on have been compiled. Developers should take great care in ensuring that modules are granted a
single purpose; if two modules strongly relate to each other, the developer should consider merging them into a single
module instead.

## Source files

Source files are where the actual code written by a developer reside. The source code files come in two different
flavors, application source code and library source code. Application source code lives in the `bin` folder, whereas the
library source code lives in the `lib` folder.

Additionally, application source files behave slightly differently from ordinary library source code files. Most
notably, an application source code file must contain the `main` function, which is the application's entry point. All
code present within the application source file also resides in an unnamed module, meaning the code can never be
imported.

Each individual source code file within a module usually represents a single concept or topic within the module. The
developer may use multiple source code files to fragment a module into small pieces, making the overall codebase easier
to comprehend.

Source files may be declared in the following manner:

```
module-directory:
  segment-a.derg
  segment-b.derg
```

There are no restrictions on dependencies between source files. Each file form its own local scope in terms of module
imports, but otherwise they all live within the same module scope. This means that a symbol defined in one file, is
immediately visible in other scopes (provided the symbol's visibility permits that). Note that if a source file imports
a module, that module's symbols will not be visible to any source file which did not explicitly import it.

An example application source file may look like the following:

```derg
use std.io

fun main()
{
    print("Hello World!")
}
```

An example library source file may look like the following:

```derg
use contants

public enum Bracket
{
    TOP,
    MIDDLE,
    BOTTOM,
}

public fun compute_tax_rate(bracket: Bracket) -> Float64
{
    if bracket == Bracket::TOP
        return TOP_TAX_RATE
    if bracket == Bracket::MIDDLE
        return MIDDLE_TAX_RATE
    return BOTTOM_TAX_RATE
}
```
