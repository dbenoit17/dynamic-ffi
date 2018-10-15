#lang racket/base

(require ffi/unsafe
         "runtime-paths.rkt")

(define dynamic-ffi (ffi-lib dynamic-ffi-lib))

(provide dynamic-ffi-run)

(define (dynamic-ffi-run . c-headers)
  (define argc (+ (length c-headers) 1))
  (define argv (cons "dynamic-ffi-rkt" c-headers))
  (define ctool 
    (get-ffi-obj "ctool_wrapper" dynamic-ffi 
     (_fun _int (_array/list _string/utf-8 argc) -> _int) 
       (lambda () (error "error"))))
  (ctool argc argv))
