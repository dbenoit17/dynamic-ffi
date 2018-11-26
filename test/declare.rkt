#lang racket/base

(require
  racket/list
  "../main.rkt"
  "../dynamic-ffi/runtime-paths.rkt")

(provide (all-defined-out))

(define ffi-data
 (dynamic-ffi-parse (build-path dynamic-ffi-tests "files/declare/int.c")))

(for ([decl ffi-data])
 (display decl)
 (newline)
 (newline))

