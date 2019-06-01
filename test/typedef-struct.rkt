#lang at-exp racket/base

(require "../unsafe.rkt")

(provide all-defined-out)

@define-inline-ffi[struct-test]{
  #include <stdint.h>
  typedef struct {
    char *name;
    uint64_t value;
  } number;

  char* names[] = {"zero", "one", "two", "three", "four", "five", "six"
                 "seven", "eight", "nine", "ten", "eleven", "twelve"};

  number add(number a, number b) {
    number c;
    c. value = a.value + b.value;
    c.name = names[c.value];
    return c;
 }

  enum test_enum {
    ONE,
    TWO
  };
}

;; demonstrate using _list-structs

(define n2 (list "two" 2))

(struct-test 'add n2 n2)

