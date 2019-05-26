#lang at-exp racket/base

(require (for-syntax racket/base)
         racket/runtime-path
         racket/format)

(provide (all-defined-out))

(define (warn-dependencies) 
 (printf "~a\n" (format @~a{

    Warning: Dynamic FFI can not find llvm and clang dependencies.
             Please install llvm and clang developent headers and libraries,
             and then run the following command:

      raco setup -p dynamic-ffi

    Installing dependencies:
    Fedora:

      sudo dnf install "~adevelopment tools" racket llvm-devel clang-devel

    Ubuntu:
    
      sudo apt-get install "build-essential" racket llvm-dev libclang-dev clang

    For more details on installing dependencies run:

      raco docs dynamic-ffi

} "@")))

(define-runtime-path dynamic-ffi-core_rkt.so
  (build-path "compiled" "native" (system-library-subpath)
    (path-add-suffix "dynamic-ffi-core_rkt" (system-type 'so-suffix))))


;; One of these could probably be used in both files.
;; The other one will quiety break everything if used
;; in the wrong place.  It's been some time and I do not
;; remember off the top of my head which was which, so
;; I'll keep them separate for now.
;; TODO double-check which version of this
;;      function should work in both files.
;; cached.rkt uses this.
(define (timestamp<=? i o)
  (<= (file-or-directory-modify-seconds i)
      (file-or-directory-modify-seconds o)))
;; make.rkt uses this
(define (timestamp<? i o)
  (< (file-or-directory-modify-seconds i)
     (file-or-directory-modify-seconds o)))
