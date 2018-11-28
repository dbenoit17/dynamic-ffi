#lang racket/base

(require racket/match
         racket/list
         "dynamic-ffi-core.rkt")

(provide declaration
         (all-from-out 'ctype-defs)
         (rename-out
           [dynamic-ffi-wrapper
           dynamic-ffi-parse]))
(module ctype-defs racket/base
 (struct declaration [name type type-string] #:transparent)
 (struct function-decl declaration [])
 (struct var-decl declaration [])
 (struct record-decl declaration [])
 (struct enum-decl declaration [])
 (struct typedef-decl declaration [])

 (struct ctype [const? volatile? literal? width] #:transparent)
 (struct ctype-void ctype [] #:transparent)
 (struct ctype-int ctype [signed?] #:transparent)
 (struct ctype-float ctype [] #:transparent)
 (struct ctype-pointer ctype [restrict? >pointee] #:transparent)
 (struct ctype-array ctype [element] #:transparent)
 (struct ctype-record ctype [members] #:transparent)
 (struct ctype-struct ctype-record [] #:transparent)
 (struct ctype-union ctype-record [] #:transparent)
 (struct ctype-function ctype [return params] #:transparent)
 (provide (all-defined-out)))

(require 'ctype-defs)

(define (dynamic-ffi-wrapper . files-list)
  (define (path->byte-string path)
    (cond [(bytes? path) path]
          [(string? path) (string->bytes/locale path)]
          [(path? path)
             (string->bytes/locale
               (path->string path))]
          [else (error "dynamic-ffi: unsupported path format: " path)]))
  (define byte-string-paths
    (for/list ([path files-list])
      (path->byte-string path)))
  (define c-decls-list
    (apply dynamic-ffi-parse
      (cons #"dynamic-ffi-parse" byte-string-paths)))
  (map make-declaration c-decls-list))

;; Where d is a list obtained from dynamic-ffi-parse
(define (make-declaration d)
  (define decl-hash
     (make-hash
       (list
         (cons 'var-decl var-decl)
         (cons 'function-decl function-decl)
         (cons 'record-decl record-decl)
         (cons 'enum-decl enum-decl)
         (cons 'unknown (λ (x)x)))))
  (define ct (decl-ctype d))
  (define dispatch (hash-ref decl-hash (decl-type-sym d)))
  (dispatch
    (decl-name d)
    (make-ctype (decl-ctype d))
    (decl-type-str d)))

;; Where t is ctype sublist of d
(define (make-ctype t)
  (define ctype-hash
     (make-hash
       (list
         (cons 'integer make-ctype-int)
         (cons 'floating make-ctype-float)
         (cons 'pointer make-ctype-pointer)
         (cons 'struct  make-ctype-struct)
         (cons 'union  make-ctype-union)
         (cons 'array  make-ctype-array)
         (cons 'function  make-ctype-function)
         (cons 'void  make-ctype-void)
         (cons 'unknown (λ (x)x)))))
  (define dispatch (hash-ref ctype-hash (raw-ctype-sym t)))
  (dispatch t))


;;List to Struct Functions

(define (decl-type-sym c-decl)
  (first c-decl))
(define (decl-type-str c-decl)
  (second c-decl))
(define (decl-name c-decl)
  (third c-decl))
(define (decl-ctype c-decl)
  (fourth c-decl))

(define (raw-ctype-sym ctype)
  (first ctype))
(define (raw-ctype-width ctype)
  (second ctype))
(define (raw-ctype-signed? ctype)
  (third ctype))
(define (raw-ctype-const? ctype)
  (fourth ctype))
(define (raw-ctype-volatile? ctype)
  (fifth ctype))
(define (raw-ctype-restrict? ctype)
  (sixth ctype))
(define (raw-ctype-literal? ctype)
  (seventh ctype))
(define (raw-ctype-fields ctype)
  (eighth ctype))
(define (raw-ctype-pointee ctype)
  (car (raw-ctype-fields ctype)))
(define (raw-ctype-function-ret ctype)
  (car (raw-ctype-fields ctype)))
(define (raw-ctype-function-args ctype)
  (cdr (raw-ctype-fields ctype)))

(define (make-ctype-int t)
 (ctype-int
   (raw-ctype-const? t)
   (raw-ctype-volatile? t)
   (raw-ctype-literal? t)
   (raw-ctype-width t)
   (raw-ctype-signed? t)))

(define (make-ctype-float t)
 (ctype-float
   (raw-ctype-const? t)
   (raw-ctype-volatile? t)
   (raw-ctype-literal? t)
   (raw-ctype-width t)))

(define (make-ctype-void t)
 (ctype-void
   (raw-ctype-const? t)
   (raw-ctype-volatile? t)
   (raw-ctype-literal? t)
   (raw-ctype-width t)))

(define (make-ctype-array t)
 (ctype-array
   (raw-ctype-const? t)
   (raw-ctype-volatile? t)
   (raw-ctype-literal? t)
   (raw-ctype-width t)
   (make-ctype (raw-ctype-pointee t))))

(define (make-ctype-pointer t)
 (ctype-pointer
   (raw-ctype-const? t)
   (raw-ctype-volatile? t)
   (raw-ctype-literal? t)
   (raw-ctype-width t)
   (raw-ctype-restrict? t)
   (make-ctype (raw-ctype-pointee t))))

(define (make-ctype-struct t)
 (ctype-struct
   (raw-ctype-const? t)
   (raw-ctype-volatile? t)
   (raw-ctype-literal? t)
   (raw-ctype-width t)
   (map make-ctype (raw-ctype-fields t))))

(define (make-ctype-union t)
 (ctype-union
   (raw-ctype-const? t)
   (raw-ctype-volatile? t)
   (raw-ctype-literal? t)
   (raw-ctype-width t)
   (map make-ctype (raw-ctype-fields t))))

(define (make-ctype-function t)
 (ctype-function
   (raw-ctype-const? t)
   (raw-ctype-volatile? t)
   (raw-ctype-literal? t)
   (raw-ctype-width t)
   (make-ctype (raw-ctype-function-ret t))
   (map make-ctype (raw-ctype-function-args t))))

