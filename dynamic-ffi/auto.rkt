#lang racket/base

(require
  ffi/unsafe
  ffi/unsafe/define
  racket/match
  (prefix-in dffi: "dynamic-ffi.rkt"))

(provide (all-defined-out) (all-from-out ffi/unsafe))

(define (make-ffi-int ct-int)
  (define width (dffi:ctype-width ct-int))
  (unless (and (modulo width 8) (<= width 64) (>= width 0))
    (error "incompatible int width: " width))
  (define ints  (vector _int8 _int16 _int32 _int64))
  (define uints  (vector _uint8 _uint16 _uint32 _uint64))
  (define category
    (if (dffi:ctype-int-signed? ct-int) ints uints))
  (define index (- (inexact->exact (log width 2)) 3))
  (vector-ref category index))

(define (make-ffi-float ct-float)
  (define width (dffi:ctype-width ct-float))
  (cond [(eq? width 32) _float]
        [(eq? width 64) _double]
        [(eq? width 80) _longdouble]
        [(eq? width 128) _longdouble]
        [else (error "incompatible float width: " width)]))

(define (make-ffi-pointer ct-pointer)
  (define pointee (dffi:ctype-pointer->pointee ct-pointer))
  (cond [(dffi:ctype-void? pointee) _pointer]
        [(and (dffi:ctype-int? pointee)
              (eq? (dffi:ctype-width pointee) 8))
         _string]
        [else (_cpointer (make-dffi-obj pointee))]))

(define (make-ffi-array ct-array)
  (define element (dffi:ctype-array-element ct-array))
  (make-array-type (make-dffi-obj element)
    (quotient (dffi:ctype-width ct-array)
              (dffi:ctype-width element))))

(define (make-ffi-struct ct-struct)
  (define struct-members
    (for/list ([mem (dffi:ctype-record-members ct-struct)])
      (make-dffi-obj mem)))
  (make-cstruct-type struct-members))

(define (make-ffi-union ct-union)
  (define union-members
    (for/list ([mem (dffi:ctype-record-members ct-union)])
      (make-dffi-obj mem)))
  (apply make-union-type union-members))

(define (make-ffi-function ct-function)
  (define params
   (for/list ([param (dffi:ctype-function-params ct-function)])
     (make-dffi-obj param)))
  (define maybe-return (dffi:ctype-function-return ct-function))
  (define return
    (if (dffi:ctype-void? maybe-return)
      _void
      (make-dffi-obj maybe-return)))
  (_cprocedure params return))

(define (make-dffi-obj ct)
 (unless (dffi:ctype? ct)
  (error "expected dynamic-ffi ctype"))
 (cond
  [(dffi:ctype-int? ct) (make-ffi-int ct)]
  [(dffi:ctype-pointer? ct) (make-ffi-pointer ct)]
  [(dffi:ctype-float? ct) (make-ffi-float ct)]
  [(dffi:ctype-struct? ct) (make-ffi-struct ct)]
  [(dffi:ctype-union? ct) (make-ffi-union ct)]
  [(dffi:ctype-array? ct) (make-ffi-array ct)]
  [(dffi:ctype-function? ct) (make-ffi-function ct)]
  [(dffi:ctype-void? ct)
   (error "void only allowed as pointer or function return")]
  [else (error "unimplemented type")]))

(define (build-ffi-obj-map header lib)
  (define ffi-data (dffi:dynamic-ffi-parse header))
  (make-hash
    (for/list ([decl ffi-data])
      (define name (dffi:declaration-name decl))
      (define type (dffi:declaration-type decl))
      (cons (string->symbol name)
            (get-ffi-obj name lib (make-dffi-obj type)
               (λ () (error "lib " lib " does not contain " name)))))))

