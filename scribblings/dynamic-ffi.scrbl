#lang scribble/manual

@title{Dynamic FFI}

@author[(author+email "David Benoit" "dbenoit@redhat.com")]

@require[@for-label[dynamic-ffi/unsafe
                    racket/base]]

@defmodule[dynamic-ffi/unsafe]{
  This module produces automatic ABI-native ffi bindings to C libraries. 

  Dynamic FFI is a native Racket extension which embeds clang/llvm to parse out declarations
  from C headers and dynamically build ffi objects with correct type/size information.
  This library is currently only availalable for GNU/Linux, but should be easily portable to
  other operating systems.  If you are experienced with building clang/llvm plugins on other
  OSes and would like to contribute, please contact the author.
  
  Warning: Like Racket's built-in ffi/unsafe module, this library allows Racket to
  use C functions and data structures that are not memory-managed by Racket.
  Racket can not provide safety guarantees by default for ffi objects created
  using this library.  Users of this library will be required to self-enforce
  memory management in C and/or manually expose ffi objects to Racket's garbage
  collector.  Extra care should be taken to ensure that C functions bound via
  this library or Racket's built-in ffi do not contain errors like buffer overflows,
  which could corrupt the runtime, cause undefined behavior, and prevent Racket from 
  providing useful error messages.

  This module uses native extensions to Racket' C runtime, and is not currently
  compatible with Racket-on-Chez.  Support for Racket-on-Chez is planned for the future.
}

@section{Defining FFIs}

@defform[(define-dynamic-ffi id lib header ...) 
         #:contracts ([id identifier?] 
                      [lib (or/c string? path?)]
                      [header (or/c string? path?)])]
Parses headers for syntax tree data and builds dynamic ffi bindings.  The resulting 
@racket[id] will be bound to a case-lambda function which takes the name of the C function
as the first argument, and the C function parameters as the rest.  If the defined @racket[id]
is called with no arguments, a hash-map of all symbols and ffi objects in the library is returned.

@racketblock[
(require dynamic-ffi/unsafe)

(define-dynamic-ffi libc
  "/usr/lib64/libc-2.29"
  "/usr/include/stdio.h")

(libc 'printf "hello world")]


Note: libc may have a different version number on your system, so you may 
need to update this example path accordingly.  In the future, support is
planned for parity with how ffi-obj handles libraries.

@defform[(define-dynamic-ffi/cached id lib header ...) 
         #:contracts ([id identifier?] 
                      [lib (or/c string? path?)]
                      [header (or/c string? path?)])]
This function behaves the same way as @racket[define-dynamic-ffi], except it caches
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

@defform[(define-inline-ffi name code ...) 
         #:contracts ([name identifier?] 
                      [code string?])]

This function is designed for use with the at-reader.

@racketblock[
(define-inline-ffi mylib
  "int add(int x, int y) {"
  "  return x + y;"
  "}")

(mylib 'add 3 4)]

