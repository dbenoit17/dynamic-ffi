#lang racket/base

(require
  racket/list
  "../main.rkt"
  "../dynamic-ffi/runtime-paths.rkt")

(provide (all-defined-out))

(define ffi-data
 (dynamic-ffi-parse "/usr/include/string.h"))

(for ([decl ffi-data])
 (display decl)
 (newline)
 (newline))

