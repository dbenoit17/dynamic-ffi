#lang racket/base

(define (ctype-sym ctype)
  (first ctype))
(define (ctype-width ctype)
  (second ctype))
(define (ctype-signed? ctype)
  (third ctype))
(define (ctype-const? ctype)
  (fourth ctype))
(define (ctype-volatile? ctype)
  (fifth ctype))
(define (ctype-restrict? ctype)
  (sixth ctype))
(define (ctype-literal? ctype)
  (seventh ctype))

(define (ctype-fields ctype)
  (eighth ctype))

(define (ctype-pointee ctype)
  (car (ctype-fields ctype)))

(define (ctype-function-ret ctype)
  (car (ctype-fields ctype)))
(define (ctype-function-args ctype)
  (cdr (ctype-fields ctype)))

(module c-types racket/base
  (provide (all-defined-out))

  (struct declaration [name type type-string] #:transparent)
  (struct function-decl declaration [])
  (struct var-decl declaration [])
  (struct struct-decl declaration [])
  (struct union-decl declaration [])

  (struct ctype [] #:transparent)
  (struct ctype-void ctype [] #:transparent)
  (struct ctype-atomic ctype [const? volatile? literal? width] #:transparent)
  (struct ctype-int ctype-atomic [signed?] #:transparent)
  (struct ctype-float ctype-atomic [] #:transparent)
  (struct ctype-pointer ctype-atomic [restrict? pointee] #:transparent)
  (struct ctype-array ctype-atomic [element] #:transparent)

  (struct ctype-composite ctype [] #:transparent)
  (struct ctype-struct ctype-composite [width fields] #:transparent)
  (struct ctype-union ctype-composite [] #:transparent)
  (struct ctype-function ctype-composite [return params] #:transparent)
)


(define (make-ctype-int t)
 (ctype-int
   (ctype-const? t)
   (ctype-volatile? t)
   (ctype-literal? t)
   (ctype-width t)
   (ctype-signed? t)))

(define (make-ctype-float t)
 (ctype-float
   (ctype-const? t)
   (ctype-volatile? t)
   (ctype-literal? t)
   (ctype-width t)))

(define (make-ctype-array t)
 (ctype-array
   (ctype-const? t)
   (ctype-volatile? t)
   (ctype-literal? t)
   (ctype-width t)
   (make-ctype (ctype-pointee t))))

(define (make-ctype-pointer t)
 (ctype-pointer
   (ctype-const? t)
   (ctype-volatile? t)
   (ctype-literal? t)
   (ctype-width t)
   (ctype-restrict? t)
   (make-ctype (ctype-pointee t))))

(define (make-ctype-struct t)
 (ctype-struct
   (ctype-width t)
   (map make-ctype (ctype-fields t))))

(define (make-ctype-function t)
 (ctype-function
   (make-ctype (ctype-function-ret t))
   (map make-ctype (ctype-function-args t))))

(require racket/match
         racket/list
         "dynamic-ffi-core.rkt"
         'c-types)

(provide (all-from-out 'c-types)
         (rename-out
           [dynamic-ffi-wrapper
           dynamic-ffi-parse]))

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

(define (make-ctype t)
  (define ctype-hash
     (make-hash
       (list
         (cons 'integer make-ctype-int)
         (cons 'floating make-ctype-float)
         (cons 'pointer make-ctype-pointer)
         (cons 'struct  make-ctype-struct)
         (cons 'array  make-ctype-array)
         (cons 'function  make-ctype-function)
         (cons 'void  (λ (x) (ctype-void)))
         (cons 'unknown (λ (x)x)))))
  (define dispatch (hash-ref ctype-hash (ctype-sym t)))
  (dispatch t))

(define (decl-type-sym c-decl)
  (first c-decl))
(define (decl-type-str c-decl)
  (second c-decl))
(define (decl-name c-decl)
  (third c-decl))
(define (decl-ctype c-decl)
  (fourth c-decl))

(define (make-declaration decl)
  (define decl-hash
     (make-hash
       (list
         (cons 'var-decl var-decl)
         (cons 'function-decl function-decl)
         (cons 'struct-decl struct-decl)
         (cons 'unknown (λ (x)x)))))
  (define ct (decl-ctype decl))
  (define dispatch (hash-ref decl-hash (decl-type-sym decl)))
  (dispatch
    (decl-name decl)
    (make-ctype (decl-ctype decl))
    (decl-type-str decl)))

