#lang scribble/manual

@title{Dynamic FFI}

@author[(author+email "David Benoit" "dbenoit@fedoraproject.org")]

@require[@for-label[dynamic-ffi/unsafe
                    racket/base]]

@defmodule[dynamic-ffi/unsafe]{
  This module produces automatic ABI-native ffi bindings to C libraries.

  Dynamic FFI is a native Racket extension which embeds clang/llvm to parse out declarations
  from C headers and dynamically build ffi objects with correct type/size information.
  This library is currently only availalable for GNU/Linux, but should be easily portable to
  other operating systems.  If you are experienced with building clang/llvm plugins on other
  OSes and would like to contribute, please contact the author.


  This module uses native extensions to Racket' C runtime, and is not currently
  compatible with Racket-on-Chez.  Support for Racket-on-Chez is planned for the future.
}

@section{Warning: Read this Section Before Use}
  Like Racket's built-in @racket[ffi/unsafe] module, this library allows Racket to
  use C functions and data structures that are not memory-managed by Racket.
  Racket can not provide safety guarantees by default for ffi objects created
  using this library.  Users of this library will be required to self-enforce
  memory management in C and/or manually expose ffi objects to Racket's garbage
  collector.  Extra care should be taken to ensure that C functions bound via
  this library or Racket's built-in ffi do not contain errors like buffer overflows,
  which could corrupt the runtime, cause undefined behavior, and prevent Racket from
  providing useful error messages.


  Users of this library should be extremely cautious.  The library
  makes it easier to call C functions from Racket, but does @bold{not} make
  it safer.

  Finally:

  @bold{Passing unsanitized user-input to any function in this library is horribly dangerous.  You should never do it under any circumstance.}

@section{Dependencies}

Dynamic FFI requires clang and llvm headers and libraries to work.
Clang versions 6 and 7 are both known to work with Dynamic FFI.
This library does not bundle clang or llvm.

To install a clang toolchain on Fedora:

@racketblock[
sudo dnf install "@development tools" racket llvm-devel clang-devel
]

To install a clang toolchain on Ubuntu:
@racketblock[
sudo apt-get install "build-essential" racket llvm-dev libclang-dev clang
]

During the raco package install, Dynamic FFI will compile itself and link
with the system clang libraries.  Dependencies should be installed before
the raco installation, or else the library will defer building itself, and will
complain about missing dependencies when you try to use it.  To finish the
installation process, install the required dependencies and run the following
command:

@racketblock[raco setup -p dynamic-ffi]

@section{Defining FFIs}



@defform[(define-dynamic-ffi id lib header ...)
         #:contracts ([id identifier?]
                      [lib (or/c string? path?)]
                      [header (or/c string? path?)])]
Parses headers for syntax tree data and builds dynamic ffi bindings.  The resulting
@racket[id] will be bound to a case-lambda function which takes the name of the C function
as the first argument, and the C function parameters as the rest.  If the defined @racket[id]
is called with no arguments, a hash-map of all symbols and ffi objects in the library is returned.

The lib argument can be a relative library created using @racket[dynamic-ffi-lib], or a hard-coded path omitting the object file extension.

@racketblock[
(require dynamic-ffi/unsafe)

(define-dynamic-ffi libc
  (dynamic-ffi-lib "libc" "6")
  "/usr/include/stdio.h")

(libc 'printf "hello world")]

@defproc[(dynamic-ffi-lib [lib (or/c string? path?)]
                          [version string?] ...)
                          (cons/c (or/c string? path?)
                                  (listof string?))]
Takes a library base name and shared object versions, and produces
a list argument which can be used in place of a hard-coded system object file path.
In the above @racket[define-dynamic-ffi] example, Racket will search for @racket[libc.so.6]
in the default system library paths.

@defform[(define-dynamic-ffi/cached id lib header ...)
         #:contracts ([id identifier?]
                      [lib (or/c string? path?)]
                      [header (or/c string? path?)])]
This form behaves the same way as @racket[define-dynamic-ffi], except it caches
static ffi bindings so that they do not need to be recomputed in future program executions.

@section{Generating Static FFIs}
This library has the ability to generate static ffi files that can be used
without the dependency on @racket[dynamic-ffi].

@defproc[(generate-static-ffi [ffi-name (or/c string? symbol?)]
                              [file (or/c string? path?)]
                              [lib-path (or/c string? path?)]
                              [headers (or/c string? path?)] ...)
                              (void)]
Generates a static ffi where identifiers corresponding to C function names
are prefixed with @racket[ffi-name] and bound to their associated ffi objects.

@defproc[(generate-mapped-static-ffi [ffi-name (or/c string? symbol?)]
                                   [file (or/c string? path?)]
                                   [lib-path (or/c string? path?)]
                                   [headers (or/c string? path?)] ...)
                                   (void)]
Generates a static ffi whose interface is equivalent to the case-lambda function
produced by @racket[define-dynamic-ffi].

@section{Inline FFIs}
This library allows users to write C functions inline, which will be compiled
at runtime and provided as a dynamic FFI.

@defform[(define-inline-ffi name code ... [#:compiler compiler]
                                          [#:compile-flags flags])
         #:contracts ([name identifier?]
                      [code string?]
                      [compiler string?]
                      [flags (or/c string? 'auto)])]

Define ffi bindings by writing inline C code.  This form is designed for use with the at-reader.

@racketblock[
(define-inline-ffi mylib
  "int add(int x, int y) {"
  "  return x + y;"
  "}")

(mylib 'add 3 4)]

Extra compile flags can be passed to @racket[define-inline-ffi], and the
default compiler can be overridden.

@racketblock[
(define-inline-ffi libm #:compile-flags "-lm" #:compiler "clang"
  "#include <math.h>"
  "double square_root(double x) {"
  "  return sqrt(x);"
  "}")
(libm 'square_root 16)]

@section{Limitations}
This library is able to generate dynamic ffi bindings for a fairly
large portion of the C language standard.  Occasionally we find
special declaration types that clang treats differently, which
the library may not have support for yet.  When such declarations are
encountered, @racket[dynamic-ffi] will emit error messages with
details about the missing functionality.  Please report any issues you
find at project issue tracker at https://github.com/dbenoit17/dynamic-ffi.

As a workaround in almost every case, @racket[define-inline-ffi] can be used to write
a wrapper around functions that @racket[dynamic-ffi] is unable to parse.




