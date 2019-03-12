#lang at-exp racket/base

(require racket/format)
(require "../experimental/inline.rkt")

(provide (all-defined-out))

@define-inline-ffi[mylib]{
  #include <stdio.h>

  int add(int x, int y) {
    printf("inside mylib: %d + %d = %d\n", x, y, x + y);
    return x + y;
  }
}

(time (mylib 'add 2 3))

