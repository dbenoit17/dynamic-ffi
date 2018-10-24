#lang racket/base

(require ffi/unsafe
         "runtime-paths.rkt")

(define dynamic-ffi (ffi-lib dynamic-ffi-lib))

(provide dynamic-ffi-parse
         dynamic-ffi-deep-parse)

(define (make-ffi-parser obj)
  (λ c-headers
    (define c_func
      (get-ffi-obj obj dynamic-ffi
       (_fun _int (_array/list
                    _string/utf-8
                    (+ (length c-headers ) 1))
           -> _int)
         (lambda () (error "error"))))
  (define argc (+ (length c-headers) 1))
  (define argv (cons "dynamic-ffi-rkt" c-headers ))
  (c_func argc argv)))

(define dynamic-ffi-parse (make-ffi-parser "ffi_parse"))
(define dynamic-ffi-deep-parse (make-ffi-parser "ffi_deep_parse"))
