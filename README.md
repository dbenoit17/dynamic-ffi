dynamic-ffi
===========

This package is not yet ready for general use.

Uses clang to parse metadata from C files into racket.

To install the package:
```
raco pkg install https://github.com/dbenoit17/dynamic-ffi.git
```

Dependencies: 
* racket
* llvm and clang development headers and libraries

Example install on Fedora:
```
sudo dnf -y install racket llvm-devel clang-devel
```

# Usage Examples

Given some example.c:
```
/* example.c */

#include <stdio.h>

typedef struct {
  int x;
  char c;
} my_struct;

int add(int x, int y) { return x + y; }

int main(int argc, char** argv) {
  my_struct st = { 4, 'd'};
  adder(1,2);
  return 0;
}
```


This racket program will parse all the function declarations
and print the number of arguments they have.  Included headers
are also parsed.

```
#lang racket/base
(require dynamic-ffi)

;; Gets function declarations and 
;; prints them as strings for now
(dynamic-ffi-parse "example.c")
```

# TODO
dynamic-ffi should return ffi objects or racket structs

