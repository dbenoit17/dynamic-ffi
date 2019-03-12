#lang racket/base

(require
  racket/set
  racket/string
  racket/format
  compiler/compiler
  openssl/sha1

  (for-syntax racket/base
              racket/syntax)
  "../runtime-paths.rkt"
  "../unsafe.rkt"
  "export.rkt"
  (prefix-in dffi: "../meta.rkt"))

(provide (all-defined-out))

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

(define (get-cached-ffi-path ffi-name lib . headers)
  (define ffi-full-string
      (format "~a ~a ~a" ffi-name lib (string-ish-join (sort headers string<?))))
  (define ffi-digest
    (sha1 (open-input-string ffi-full-string)))
  (unless (directory-exists? ffi-cache-path)
    (make-directory ffi-cache-path))
  (build-path ffi-cache-path (format "~a.~a.ffi.rkt" ffi-name ffi-digest)))

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

(define (lib-equal? ffi-name cached-file-path lib)
  (define cached-lib
    (dynamic-require cached-file-path
      (string->symbol (format "~a-library" ffi-name))
      (λ () #f)))
  (define result (equal? lib cached-lib))
  (debug-msg (format "lib-equal? ~a\n" result))
  result)

; also defined in make.rkt
(define (timestamp<=? i o)
  (<= (file-or-directory-modify-seconds i)
      (file-or-directory-modify-seconds o)))

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
     #'(define id
        (let* ([cached-file-path (get-cached-ffi-path 'id lib header ...)]
               [ffi-obj-map
           (cond [(cache-valid? 'id cached-file-path lib header ...)
                  (dynamic-require cached-file-path 'id)]
             [else
               (let ([ffi-data (dffi:dynamic-ffi-parse header ...)])
                 (cache-ffi! ffi-data 'id cached-file-path lib header ...)
                 ((compile-zos #t)
                    (list cached-file-path) 'auto)
                 (build-ffi-obj-map2 ffi-data lib header ...))])])
           (case-lambda
             [() ffi-obj-map]
             [(sym)
              (let ([obj (hash-ref ffi-obj-map sym)])
                (if (procedure? obj) (obj) obj))]
             [(sym . args)
              (let ([obj (hash-ref ffi-obj-map sym)])
                (apply obj args))])))]))

