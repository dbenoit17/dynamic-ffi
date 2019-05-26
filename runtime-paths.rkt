#lang racket/base


(require (for-syntax racket/base)
         racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path dynamic-ffi.c
  (build-path "core" "dynamic-ffi.c"))

(define-runtime-path dynamic-ffi.3m.c
  (build-path "core" "dynamic-ffi.3m.c"))

(define-runtime-path wrap-fork.c
  (build-path "core" "wrap-fork.c"))

(define-runtime-path clang-plugin.cc
  (build-path "core" "clang-plugin.cc"))

(define-runtime-path clang-export.c
  (build-path "core" "clang-export.c"))

(define-runtime-path compiled-native-dir
 (build-path "compiled" "native"))

(define-runtime-path compiled-dir
 (build-path "compiled"))

(define-runtime-path dynamic-extension-dir
 (build-path "private" "native" (system-library-subpath)))

(define-runtime-path shared-object-dir
 (build-path "private" "native"))

(define-runtime-path dynamic-ffi_3m.o
  (build-path "private" "native" "dynamic-ffi_3m.o"))

(define-runtime-path wrap-fork.so
  (build-path "private" "native" (system-library-subpath #f)
    (path-add-suffix "wrap-fork" (system-type 'so-suffix))))

(define-runtime-path clang-plugin.so
  (build-path "private" "native" (system-library-subpath #f)
    (path-add-suffix "clang-plugin" (system-type 'so-suffix))))

(define-runtime-path clang-export.so
  (build-path "private" "native" (system-library-subpath #f)
    (path-add-suffix "clang-export" (system-type 'so-suffix))))

(define-runtime-path clang-export-lib
  (build-path "private" "native" "clang-export"))

;; Test Paths
(define-runtime-path dynamic-ffi-tests
  (build-path "test"))



