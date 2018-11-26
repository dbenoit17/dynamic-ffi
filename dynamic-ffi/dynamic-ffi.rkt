#lang racket/base


(require racket/match)
(module c-types racket/base
  (provide (all-defined-out))
  (struct declaration [name type type-string] #:transparent)
  (struct function-decl declaration [])
  (struct var-decl declaration [])
  (struct struct-decl declaration [])
  (struct union-decl declaration [])

  (struct ctype [] #:transparent)
  (struct ctype-atomic ctype [const volatile restrict] #:transparent)
  (struct ctype-int ctype-atomic [width] #:transparent)
  (struct ctype-signed-int ctype-int [] #:transparent)
  (struct ctype-unsigned-int ctype-int [] #:transparent)
  (struct ctype-float ctype-atomic [width] #:transparent)
  (struct ctype-signed-float ctype-float [] #:transparent)
  (struct ctype-unsigned-float ctype-float [] #:transparent)
  (struct ctype-composite ctype [] #:transparent)
  (struct ctype-struct ctype-composite [] #:transparent)
  (struct ctype-union ctype-composite [] #:transparent)
  (struct ctype-pointer ctype-composite [] #:transparent)
  (struct ctype-function ctype-composite [] #:transparent)
)

(require racket/match
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

(define (decl-type-sym c-decl)
  (car c-decl))
(define (decl-name c-decl)
  (cadr c-decl))
(define (decl-ctype c-decl)
  (caddr c-decl))
(define (make-declaration decl)
  (define type-sym (decl-type-sym decl))
  (match type-sym
    ["int" (declaration
            (decl-name decl)
            (decl-ctype decl)
            type-sym)]
    ["uint64_t" (declaration
            (decl-name decl)
            (decl-ctype decl)
            type-sym)]
    [_ decl]))



