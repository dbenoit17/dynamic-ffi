#lang racket/base

(require
  racket/list
  "../main.rkt"
  "../dynamic-ffi/runtime-paths.rkt")

(provide (all-defined-out))

(define ffi-data
 (dynamic-ffi-parse (build-path dynamic-ffi-tests "c-mini-test/c-mini-test.h")))

(for ([decl ffi-data])
 (display decl)
 (newline)
 (newline))

