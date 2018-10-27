#lang racket/base

(require racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path header-parse.cc "ffi/header-parse.cc")
(define-runtime-path header-parse.so "ffi/header-parse.so")
(define-runtime-path header-parse-lib "ffi/header-parse")
