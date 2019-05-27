#lang info
(define collection "dynamic-ffi")
(define deps '("base" "rackunit-lib"))
(define build-deps '("racket-doc" "rackunit-lib" "at-exp-lib"
                     "scribble-lib"))
(define scribblings '(("scribblings/dynamic-ffi.scrbl")))
(define pkg-desc "")
(define version "1.0")
(define pkg-authors '(dbenoit))
(define post-install-collection "make.rkt")
(define test-omit-paths
  '("test/inline.rkt"
    "test/manual/generate.rkt"
    "test/libc.rkt"
    "test/typedef-struct.rkt"
    "test/assembly.rkt"))
