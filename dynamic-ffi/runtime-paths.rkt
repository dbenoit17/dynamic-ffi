#lang racket/base


(require (for-syntax racket/base)
         racket/runtime-path)

(provide (all-defined-out))


(define-runtime-path dynamic-ffi.c
  (build-path ".." "core" "dynamic-ffi.c"))

(define-runtime-path dynamic-ffi.3m.c
  (build-path ".." "core" "dynamic-ffi.3m.c"))

(define-runtime-path ffi-plugin.cc
  (build-path ".." "core" "ffi-plugin.cc"))

(define-runtime-path header-parse.cc
  (build-path ".." "core" "header-parse.cc"))

(define-runtime-path dynamic-extension-dir
 (build-path "compiled" "native" (system-library-subpath)))

(define-runtime-path shared-object-dir
 (build-path "compiled" "native"))

(define-runtime-path dynamic-ffi_3m.o
  (build-path "compiled" "native" "dynamic-ffi_3m.o"))

(define-runtime-path ffi-plugin.so
  (build-path "compiled" "native"
    (path-add-suffix "ffi-plugin" (system-type 'so-suffix))))

(define-runtime-path header-parse.so
  (build-path "compiled" "native"
    (path-add-suffix "header-parse" (system-type 'so-suffix))))

(define-runtime-path header-parse-lib
  (build-path "compiled" "native" "header-parse"))

(define-runtime-path dynamic-ffi-core_rkt.so
  (build-path "compiled" "native" (system-library-subpath)
    (path-add-suffix "dynamic-ffi-core_rkt" (system-type 'so-suffix))))


;; Test Paths
(define-runtime-path dynamic-ffi-tests
  (build-path ".." "test"))

