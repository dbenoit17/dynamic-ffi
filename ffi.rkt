;; This module provides the basic functionality
;; for dynamic-ffi.  It converts AST metadata
;; from meta.rkt into racket's builtin ffi objects.

#lang racket/base

(require
  (rename-in
    ffi/unsafe [-> ffi->])
  racket/system
  racket/contract
  (for-syntax racket/base
              racket/syntax
              syntax/parse)
  (prefix-in dffi: "meta.rkt"))

(provide
  define-dynamic-ffi
  build-ffi-obj-map
  (contract-out
    [dynamic-ffi-lib
     (-> (or/c string? path?) string? ... 
         (cons/c (or/c string? path?) 
                 (listof string?)))]))

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
        [(eq? width 64) _double*]
        [(eq? width 80) _longdouble]
        [(eq? width 128) _longdouble]
        [else (error "incompatible float width: " width)]))

(define (make-ffi-pointer ct-pointer)
  (define pointee (dffi:ctype-pointer->pointee ct-pointer))
  (cond [(dffi:ctype-void? pointee) _pointer]
        [(and (dffi:ctype-int? pointee)
              (eq? (dffi:ctype-width pointee) 8))
         _string]
        ;; all pointers opaque for now to
        ;; prevent type conflicts 
        [else _pointer]))
        ;; would be cool to do it this way
        ;;[else (_cpointer (make-dffi-obj pointee))]))

(define (make-ffi-array ct-array)
  (define element (dffi:ctype-array-element ct-array))
  (_array (make-dffi-obj element)
    (quotient (dffi:ctype-width ct-array)
              (dffi:ctype-width element))))

(define (make-ffi-struct ct-struct)
  (define struct-members
    (for/list ([mem (dffi:ctype-record-members ct-struct)])
      (make-dffi-obj mem)))
  (if (null? struct-members) #f
  (apply _list-struct struct-members)))

(define (make-ffi-union ct-union)
  (define union-members
    (for/list ([mem (dffi:ctype-record-members ct-union)])
      (make-dffi-obj mem)))
  (if (null? union-members) #f
    (apply _union union-members)))

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
  [(dffi:ctype-void? ct) _void]
  [else (error "unimplemented type")]))

;; This should probably return a custom type
;; instead of a list.  See the description
;; in the module docs for how it is used.
(define (dynamic-ffi-lib lib . versions)
  (cons lib versions))

;; Take a shared library and headers.
;; Build a mapping of function symbols
;; with their corresponding ffi objects.
(define (build-ffi-obj-map ffi-data lib . headers)
  (define ffi-library 
    (cond [(ffi-lib? lib) lib]
          [(or (string? lib) (path? lib))
           (ffi-lib lib)]
          [(pair? lib)
           (ffi-lib (car lib) (cdr lib))]))
  (define pairs
    (for/list ([decl ffi-data])
      (define name (dffi:declaration-name decl))
      (define type (dffi:declaration-type decl))
      (define ffi-obj
        (cond [(= (string-length name) 0) #f]
              [(dffi:enum-decl? decl)
               (dffi:declaration-literal-value decl)]
              [(or (dffi:record-decl? decl)
                   (dffi:typedef-decl? decl))
               (make-dffi-obj type)]
              [(or (dffi:function-decl? decl) 
                   (dffi:var-decl? decl))
               (get-ffi-obj name ffi-library (make-dffi-obj type)
                 (λ () 
                   (printf "warning: ~a does not contain declared symbol ~a\n" 
                    lib name) #f))]
              [else
                 (printf "warning: unimplemented delcaration type: ~a\n" decl)]))
      (cons (string->symbol name) ffi-obj)))
  (make-hash
    (filter (λ (x) (cdr x)) pairs)))

(define-syntax (define-dynamic-ffi stx)
  (syntax-parse stx
    [(_ id:id lib header ...)
     #:declare lib (expr/c #'(or/c string? path? ffi-lib? 
                               (cons/c (or/c string? path?)
                               (listof string?))))
     #:declare header (expr/c #'(or/c string? path?))
       #'(define id
           (let* ([ffi-data (dffi:dynamic-ffi-parse header ...)]
                  [ffi-obj-map (build-ffi-obj-map ffi-data lib header ...)])
              (case-lambda
                [() ffi-obj-map]
                [(sym)
                 (let ([obj (hash-ref ffi-obj-map sym)])
                   (if (procedure? obj) (obj) obj))]
                [(sym . args)
                 (let ([obj (hash-ref ffi-obj-map sym)])
                   (apply obj args))])))]))
