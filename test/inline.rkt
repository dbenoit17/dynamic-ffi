#lang at-exp racket/base

(require rackunit
         rackunit/text-ui
         dynamic-ffi/unsafe)

@define-inline-ffi[mylib]{
  #include <stdio.h>
  #include <string.h>

  int int_add(int x, int y) {
    return x + y;
  }

  int int_sub(int x, int y) {
    return x - y;
  }

  int int_mult(int x, int y) {
    return x * y;
  }

  int int_div(int x, int y) {
    return x / y;
  }

  int int_mod(int x, int y) {
    return x % y;
  }

  int add_ten_args(int a, int b, int c, int d, int e, 
                   int f, int g, int h, int i, int j) {
    return a + b + c + d + e + f + g + h + i + j;
  }

  double double_add(double x, double y) {
    return x + y;
  }

  double double_sub(double x, double y) {
    return x - y;
  }

  double double_mult(double x, double y) {
    return x * y;
  }

  double double_div(double x, double y) {
    return x / y;
  }

  char* shorten(char* string) {
    int len = strlen(string);
    string[len/2] = '\0';
    return string;
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

(define tests
  (test-suite
    "test inline ffi"
    (test-case
      "check ffi integer addition"
      (let ([x (random 10000)]
            [y (random 10000)])
        (check-equal? (mylib 'int_add x y) (+ x y))))
    (test-case
      "check ffi integer subtraction"
      (let ([x (random 10000)]
            [y (random 10000)])
        (check-equal? (mylib 'int_sub x y) (- x y))))
    (test-case
      "check ffi integer multiplication"
      (let ([x (random 10000)]
            [y (random 10000)])
        (check-equal? (mylib 'int_mult x y) (* x y))))
    (test-case
      "check ffi integer division"
      (let ([x (random 10000)]
            [y (random 10000)])
        (check-equal? (mylib 'int_div x y) (quotient x y))))
    (test-case
      "check ffi integer modulo"
      (let ([x (random 10000)]
            [y (random 10000)])
        (check-equal? (mylib 'int_mod x y) (remainder x y))))
    ;; I was pleasantly surprised these floating point tests
    ;; work.  Are Racket's real numbers implemented with
    ;; IEEE floats?  If these ever break, it's likely
    ;; a difference in rounding between Racket and IEEE,
    ;; and its probably ok to just delete the tests.
    (test-case
      "check ffi floating double addition"
      (let ([x (random)]
            [y (random)])
        (check-equal? (mylib 'double_add x y) (+ x y))))
    (test-case
      "check ffi floating double subtraction"
      (let ([x (random)]
            [y (random)])
        (check-equal? (mylib 'double_sub x y) (- x y))))
    (test-case
      "check ffi floating double multiplication"
      (let ([x (random)]
            [y (random)])
        (check-equal? (mylib 'double_mult x y) (* x y))))
    (test-case
      "check ffi floating double division"
      (let ([x (random)]
            [y (random)])
        (check-equal? (mylib 'double_div x y) (/ x y))))
    (test-case
      "check ffi ten args"
      (let ([nums (build-list 10 (λ (x) (random 10000)))])
        (check-equal? (apply mylib (cons 'add_ten_args nums)) (apply + nums))))
    (test-case
      "check ffi inline assembly language"
      (let ([x (random 10000)]
            [y (random 10000)])
        (check-equal? (oh-my-racket! 'add x y ) (+ x y))))
    (test-case
     "shorten string"
     (check-equal?  (mylib 'shorten "hello world") "hello"))
))

(run-tests tests)

