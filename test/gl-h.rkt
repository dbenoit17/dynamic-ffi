#lang racket/base

(require
  racket/list
  "../unsafe.rkt")

;; Builds an auto-ffi
(define-mapped-ffi gl.h "/usr/lib64/libGL" "/usr/include/GL/gl.h")

(provide (all-defined-out))

