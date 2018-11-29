#lang racket/base

(require
  racket/list
  "../dynamic-ffi/unsafe.rkt")


(define string1 "hello")
(define string2 " world")
(define string1+string2
  (string-append string1 string2))


;; Builds an auto-ffi
(define-dynamic-ffi gl.h "/usr/include/GL/gl.h" "/usr/lib64/libGL")

(provide (all-defined-out))

