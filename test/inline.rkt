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

@define-inline-ffi[oh-my-racket!]{
  int add(int x, int y) {
    __asm__("addl %%ebx, %%eax;"
            :"=r"(y)
            :"a"(x),"b"(y));
    return y;
  }
}

(define a (mylib 'add 2 3))
(define b (oh-my-racket! 'add 2 3))

(printf "\nreturn from mylib: ~a\n" a)
(printf "return from oh-my-racket!: ~a\n" b)
(printf "equal? ~a\n" (eq? a b))

