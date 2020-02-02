dynamic-ffi
===========

Gather top-level declaration syntax-tree data from C
headers via clang.

This module is currently only available for GNU/Linux and macOS.

### Warning: Read this Section Before Use
Like Racket's built-in `ffi/unsafe` module, this library allows Racket to
use C functions and data structures that are not memory-managed by Racket.
Racket cannot provide safety guarantees by default for FFI objects created
using this library.  Users of this library will be required to self-enforce
memory management in C and/or manually expose FFI objects to Racket's garbage
collector.  Extra care should be taken to ensure that C functions bound via
this library or Racket's built-in FFI do not contain errors like buffer overflows,
which could corrupt the runtime, cause undefined behavior, and prevent Racket from
providing useful error messages.


Users of this library should be extremely cautious.  The library
makes it easier to call C functions from Racket, but does **not** make
it safer.

Finally:

**Passing unsanitized user-input to any function in this library is horribly dangerous.  You should never do it under any circumstance.**

## Installation
To install the package:
```
raco pkg install dynamic-ffi
```

### Dependencies:
* racket
* llvm and clang development headers and libraries

Fedora:
```
sudo dnf install "@development tools" racket llvm-devel clang-devel
```
Ubuntu:
```
sudo apt-get install "build-essential" racket llvm-dev libclang-dev clang
```

macOS (via [Homebrew](https://brew.sh/)):

```
brew install llvm
# then follow instructions emitted by Homebrew to setup paths
```

## Documentaion
Documentation is available at https://docs.racket-lang.org/dynamic-ffi/index.html, or locally after the package is installed by running the following command:

```
raco docs dynamic-ffi
```

## Future Scoping
I hope to also write bindings to the dynamic FFI core from
other languages like Rust and Python.
