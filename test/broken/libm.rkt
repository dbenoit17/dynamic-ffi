#lang racket/base

(require rackunit
         rackunit/text-ui
         dynamic-ffi/unsafe)

(provide (all-defined-out))

;; (1)
;; This one finds some kind of unrecognized
;; function prototypes
(define-dynamic-ffi libm
   (dynamic-ffi-lib "libm" "6")
   "/usr/include/bits/mathcalls.h")


;; (2)
;; This one does not get much at all,
;; since deep-parse is not called by default.
;; deep-parse would not help with the 
(define-dynamic-ffi libm2
  (dynamic-ffi-lib "libm" "6")
  "/usr/include/math.h")

