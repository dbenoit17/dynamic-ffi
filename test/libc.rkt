#lang racket/base

(require rackunit
         rackunit/text-ui
         dynamic-ffi/unsafe)

(define-dynamic-ffi libc
  (dynamic-ffi-lib "libc" "6")
  "/usr/include/string.h")

(define-dynamic-ffi/cached libc/cached
  (dynamic-ffi-lib "libc" "6")
  "/usr/include/string.h")


(define tests
  (test-suite
    "test libc"
    (test-case
      "check strlen"
      (let ([str (list->bytes (build-list (random 10000) (λ (x) (random 32 125))))])
        (check-equal? (libc 'strlen str) (bytes-length str))))
    (test-case
      "check strlen cached"
      (let ([str (list->bytes (build-list (random 10000) (λ (x) (random 32 125))))])
        (check-equal? (libc/cached 'strlen str) (bytes-length str))))))

(run-tests tests)
