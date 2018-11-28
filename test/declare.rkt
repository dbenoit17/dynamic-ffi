#lang racket/base

(require
  racket/list
  "../main.rkt"
  "../dynamic-ffi/runtime-paths.rkt"
  "../dynamic-ffi/auto.rkt")

(provide (all-defined-out))

(define ffi-data
 (dffi-dynamic-ffi-parse (build-path dynamic-ffi-tests "files/declare/struct.c")))


(for ([decl ffi-data])
 (display decl)
 (newline)
 (newline))

(define x (make-dffi-obj (dffi-declaration-type (car ffi-data))))
(for ([decl ffi-data])
 (display (make-dffi-obj (dffi-declaration-type decl)))
 (newline)
 (newline))

