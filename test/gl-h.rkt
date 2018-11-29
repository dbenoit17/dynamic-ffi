#lang racket/base

(require
  racket/list
  "../dynamic-ffi/unsafe.rkt")

;; Builds an auto-ffi
(define-dynamic-ffi gl.h "/usr/lib64/libGL" "/usr/include/GL/gl.h")

(provide (all-defined-out))

