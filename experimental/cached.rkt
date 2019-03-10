#lang racket/base

(require
  racket/set
  compiler/compiler
  (for-syntax racket/base
              racket/syntax)
  "../runtime-paths.rkt"
  "../unsafe.rkt"
  "export.rkt"
  (prefix-in dffi: "../meta.rkt"))

(provide (all-defined-out))

(define (get-cached-ffi-path ffi-name)
  (unless (directory-exists? ffi-cache-path)
    (make-directory ffi-cache-path))
  (build-path ffi-cache-path (format "~a.ffi.rkt" ffi-name)))

(define (headers-equal? ffi-name headers)
  (define cached-headers
    (dynamic-require (get-cached-ffi-path ffi-name)
      (string->symbol (format "~a-headers" ffi-name))
      (λ () #f)))
  (and (= (length headers) (length cached-headers))
    (for/and ([header headers])
     (member header cached-headers))
    #t))

(define (lib-equal? ffi-name lib)
  (define cached-lib
    (dynamic-require (get-cached-ffi-path ffi-name)
      (string->symbol (format "~a-library" ffi-name))
      (λ () #f)))
  (equal? lib cached-lib))

; also defined in make.rkt
(define (timestamp<? i o)
  (< (file-or-directory-modify-seconds i)
     (file-or-directory-modify-seconds o)))

(define (timestamps-valid? ffi-name lib headers)
  (define suffix (system-type 'so-suffix))
  (for/and ([file (cons (format "~a~a" lib suffix) headers)])
    (timestamp<? file (get-cached-ffi-path ffi-name))))

(define (cache-valid? ffi-name lib . headers)
  (and (file-exists? (get-cached-ffi-path ffi-name))
    (and (lib-equal? ffi-name lib)
       (and (headers-equal? ffi-name headers))
         (timestamps-valid? ffi-name lib headers))))

(define (cache-ffi! ffi-data ffi-name lib . headers)
  (apply create-mapped-static-ffi ffi-data
    (get-cached-ffi-path ffi-name) ffi-name lib headers))

(define (build-ffi-obj-map2 ffi-data lib . headers)
  (unless (for/or ([ext '(".so" ".dylib" ".dll")])
            (file-exists? (string-append lib ext)))
    (error "file does not exist: " (string-append lib ".so")))
  (define pairs
    (for/list ([decl ffi-data])
      (define name (dffi:declaration-name decl))
      (define type (dffi:declaration-type decl))
      (define ffi-obj
        (if (dffi:enum-decl? decl)
          (dffi:declaration-literal-value decl)
          (get-ffi-obj name lib (make-dffi-obj type)
            (λ () (printf "warning: ~a does not contain declared symbol ~a\n" lib name) #f))))
      (cons (string->symbol name) ffi-obj)))
  (make-hash
    (filter (λ (x) (cdr x)) pairs)))

(define-syntax (define-dynamic-ffi/cached stx)
  (syntax-case stx ()
    [(_ id lib header ...)
     #'(let ([ns (current-namespace)]
             [mapped-symbols (list->set (namespace-mapped-symbols))])
           (let ([ffi-obj-map
                  (cond [(cache-valid? 'id lib header ...)
                         (dynamic-require (get-cached-ffi-path 'id) 'id)]
                    [else
                      (let ([ffi-data (dffi:dynamic-ffi-parse header ...)])
                        (cache-ffi! ffi-data 'id lib header ...)
                        ((compile-zos #t)
                           (list (get-cached-ffi-path 'id)) 'auto)
                        (build-ffi-obj-map2 ffi-data lib header ...))])])
             (for ([kv (hash->list ffi-obj-map)])
               (define new-id (string->symbol (format "~a-~a" 'id (car kv))))
               (when (set-member? mapped-symbols new-id)
                 (error (format "error: define-dynamic-ffi/cached: symbol '~a already exists in current namespace" new-id)))
               (namespace-set-variable-value! new-id (cdr kv) #t ns #t))))]))
