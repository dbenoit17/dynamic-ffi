#lang racket/base


(require (for-syntax racket/base)
         racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path dynamic-ffi.c
  (build-path "core" "dynamic-ffi.c"))

(define-runtime-path dynamic-ffi.3m.c
  (build-path "core" "dynamic-ffi.3m.c"))

(define-runtime-path clang-plugin.cc
  (build-path "core" "clang-plugin.cc"))

(define-runtime-path clang-export.cc
  (build-path "core" "clang-export.cc"))

(define-runtime-path dynamic-extension-dir
 (build-path "compiled" "native" (system-library-subpath)))

(define-runtime-path shared-object-dir
 (build-path "compiled" "native"))

(define-runtime-path dynamic-ffi_3m.o
  (build-path "compiled" "native" "dynamic-ffi_3m.o"))

(define-runtime-path clang-plugin.so
  (build-path "compiled" "native"
    (path-add-suffix "clang-plugin" (system-type 'so-suffix))))

(define-runtime-path clang-export.so
  (build-path "compiled" "native"
    (path-add-suffix "clang-export" (system-type 'so-suffix))))

(define-runtime-path clang-export-lib
  (build-path "compiled" "native" "clang-export"))

(define-runtime-path dynamic-ffi-core_rkt.so
  (build-path "compiled" "native" (system-library-subpath)
    (path-add-suffix "dynamic-ffi-core_rkt" (system-type 'so-suffix))))


;; Test Paths
(define-runtime-path dynamic-ffi-tests
  (build-path "test"))

