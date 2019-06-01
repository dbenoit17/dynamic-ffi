#lang at-exp racket/base

(require "../unsafe.rkt")

@define-inline-ffi[struct-test]{
  #include <stdlib.h>
  #include <stdio.h>
  #include <stdint.h>
  typedef struct {
    char *name;
    uint64_t value;
  } number;

  char* names[] = {"zero", "one", "two", "three", "four", "five", "six"
                 "seven", "eight", "nine", "ten", "eleven", "twelve"};

  number add(number a, number b) {
    number c;
    c.value = a.value + b.value;
    if (c.value >12)  {
      fprintf(stderr, "error: this example can only count to twelve...\n");
      exit(1);
    }
    c.name = names[c.value];
    return c;
 }
}

;; demonstrate using _list-structs

(define n2 (list "two" 2))

(define n7 (list "seven" 7))

(printf "add(n2, n2): ~a\n" (struct-test 'add n2 n2))
(printf "add(n7, n7): ~a\n" (struct-test 'add n7 n7))


