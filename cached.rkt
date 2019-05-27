;; This module provides functions for
;; caching and loading static ffi bindings.

#lang racket/base

(require
  racket/set
  racket/string
  racket/format
  compiler/compiler
  openssl/sha1
  racket/runtime-path
  racket/contract
  (for-syntax racket/base
              syntax/parse
              racket/syntax)
  "ffi.rkt"
  "export.rkt"
  "common.rkt"
  (prefix-in dffi: "meta.rkt"))

(provide 
  define-dynamic-ffi/cached
  ffi-cache-path
  (contract-out
    [timestamp<=? 
     (-> (or/c string? path?) (or/c string? path?) any)]))

(module ffi-lib racket/base
  (require ffi/unsafe)
  (provide ffi-lib))
(require 'ffi-lib)

(define-runtime-path ffi-cache-path
  (build-path "compiled" "ffi-cache"))

(define __debug #f)

(define (debug-file-exists? file)
  (define result (file-exists? file))
  (debug-msg (format "file-exists? ~a ~a\n" file result))
  result)

(define (debug-msg msg)
  (when __debug (fprintf (current-error-port) "~a" msg)))

(define (string-ish-join l)
  (string-join
    (for/list ([e l])
      (format "~a" e))))

(define (escape-for-path str)
  (string-replace str "/" "_"))
                  
(define (get-cached-ffi-path ffi-name lib . headers)
  (define ffi-full-string
      (format "~a ~a ~a" ffi-name lib (string-ish-join (sort headers string<?))))
  (define ffi-digest
    (sha1 (open-input-string ffi-full-string)))
  (unless (directory-exists? ffi-cache-path)
    (make-directory ffi-cache-path))
  (build-path ffi-cache-path 
    (format "~a.~a.ffi.rkt" (escape-for-path (format "~a" ffi-name)) ffi-digest)))

;; Check if a cached ffi uses the correct headers
(define (headers-equal? ffi-name cached-file-path headers)
  (define cached-headers
    (dynamic-require cached-file-path
      (string->symbol (format "~a-headers" ffi-name))
      (λ () '())))
  (define result
    (equal? (sort (map ~a headers) string<?)
            (sort (map ~a cached-headers) string<?)))
  (debug-msg (format "header-equal? ~a\n" result))
  result)

;; Check if a cached ffi uses the correct lib
(define (lib-equal? ffi-name cached-file-path lib)
  (define cached-lib
    (dynamic-require cached-file-path
      (string->symbol (format "~a-ffi-lib" ffi-name))
      (λ () #f)))
  (define result (equal? (ffi-lib lib) cached-lib))
  (debug-msg (format "lib-equal? ~a = ~a : ~a\n" lib cached-lib result))
  result)

(define (timestamps-valid? ffi-name cached-file-path lib headers)
  (define suffix (system-type 'so-suffix))
  (for/and ([file (cons (format "~a~a" lib suffix) headers)])
    (timestamp<=? file cached-file-path)))

(define (cache-valid? ffi-name cached-file-path lib . headers)
  (and (debug-file-exists? cached-file-path)
    (and (lib-equal? ffi-name cached-file-path lib)
       (and (headers-equal? ffi-name cached-file-path headers))
         (timestamps-valid? ffi-name cached-file-path lib headers))))

(define (cache-ffi! ffi-data ffi-name cached-file-path lib . headers)
  (apply create-mapped-static-ffi ffi-data
    cached-file-path ffi-name lib headers))

(define-syntax (define-dynamic-ffi/cached stx)
  (syntax-parse stx
    [(_ id:id lib header ...)
     #:declare lib (expr/c #'(or/c string? path? 
                               (cons/c (or/c string? path?)
                                       (listof string?))))
     #:declare header (expr/c #'(or/c string? path?))
     #'(define id
        (let* ([cached-file-path (get-cached-ffi-path 'id lib.c header ...)]
               [ffi-obj-map
           (cond [(cache-valid? 'id cached-file-path lib.c header.c ...)
                  ((dynamic-require cached-file-path 'id))]
             [else
               (let ([ffi-data (dffi:dynamic-ffi-parse header ...)])
                 (cache-ffi! ffi-data 'id cached-file-path lib.c header.c ...)
                 ((compile-zos #t)
                    (list cached-file-path) 'auto)
                 (build-ffi-obj-map ffi-data lib.c header.c ...))])])
           (case-lambda
             [() ffi-obj-map]
             [(sym)
              (let ([obj (hash-ref ffi-obj-map sym)])
                (if (procedure? obj) (obj) obj))]
             [(sym . args)
              (let ([obj (hash-ref ffi-obj-map sym)])
                (apply obj args))])))]))

