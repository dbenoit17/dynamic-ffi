#lang at-exp racket/base

(require racket/format
  racket/syntax
  racket/string
  (for-syntax racket/base
              syntax/parse))

(define asm/c @~a{
  int ~a(~a) {
    __asm__(~a
            :~a
            :~a);
    return ~a;
  }
})

(define-syntax (define-inline-ffi/asm stx)
  (syntax-parse stx
    [(_ id ([var reg] ...) [ret] instrs ...)
     #'(define id 
         (format asm/c 'id (string-join (list (~a var) ...) ",") 
           (string-append instrs ... ) ret (string-join (list (format "\"~a\"(~a)" reg var) ...) ",") ret))]))

(define-inline-ffi/asm add (['x 'eax] ['y 'ebx]) ['y] "addl %%eax %%ebx;")

(printf add)
