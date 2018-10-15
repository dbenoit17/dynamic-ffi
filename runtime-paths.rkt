#lang racket/base

(require racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path dynamic-ffi.cc "ffi/dynamic-ffi.cc")
(define-runtime-path dynamic-ffi.so "ffi/dynamic-ffi.so")
(define-runtime-path dynamic-ffi-lib "ffi/dynamic-ffi")
