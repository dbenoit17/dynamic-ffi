#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))


(define (warn-undefined-symbol sym)
  (lambda ()
    (fprintf (current-error-port)
      "warning: ~a does not contain symbol ~~a" sym)))

(define ~a 
 ~a)

(define (~a-ref elem)
  (hash-ref ~a elem))

(define (~a-fncall elem . params)
  (apply (hash-ref ~a elem params)))
  
