dynamic-ffi
===========

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

(define function-defs (parse-header "example.c"))

(printf "~a\n" function-defs)
```

# Todo
Investigate the ability to clang API calls directly to the racket ffi:


