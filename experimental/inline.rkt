#lang racket/base

(require openssl/sha1
         racket/system
         (for-syntax
           racket/base
           racket/syntax)
         "../runtime-paths.rkt"
         "cached.rkt")

(provide (all-defined-out))

(define default-flags
  "-shared -Wl,-undefined,dynamic_lookup -fPIC -O2")

(define (get-cached-c-source-path c-code [key 'ffiobj])
  (define ffi-digest
    (sha1 (open-input-string (format "~a ~a" key c-code))))
  (unless (directory-exists? ffi-cache-path)
    (make-directory ffi-cache-path))
  (build-path ffi-cache-path (format "~a.~a.rkt.c" key ffi-digest)))

(define (get-cached-c-obj-path c-code [key 'ffiobj])
  (format "~a.so" (get-cached-c-source-path c-code key)))

(define (cache-inline-c c-code [key 'ffiobj])
  (with-output-to-file (get-cached-c-source-path c-code key)
    (λ () (printf "~a\n" c-code))))

(define (find-first-compiler compilers)
  (if (null? compilers) (error "could not find a C compiler on PATH")
    (let ([found-compiler (find-executable-path (format "~a" (car compilers)))])
      (if found-compiler found-compiler
        (find-first-compiler (cdr compilers))))))

(define (cache-compile-inline-c c-code
                                #:key [key 'ffiobj]
                                #:compiler [c 'auto]
                                #:compile-flags [flags default-flags])
  (define compiler
    (if (eq? c 'auto)
      (find-first-compiler (list 'gcc 'clang))
      (find-executable-path c)))
  (define source (get-cached-c-source-path c-code key))
  (unless compiler
    (error "could not find compiler on path"))
  (define object-file (get-cached-c-obj-path c-code key))
  (define cmd (format "~a ~a -o ~a ~a" compiler source object-file flags))
  (system cmd))

(define-syntax-rule (inline key code ...)
  (list key (string-append code ...)))

(define-syntax (define-inline-ffi stx)
  (syntax-case stx ()
    [(_ key code ...)
     (with-syntax*
       ([name  (format-id #'key "~a" (syntax->datum #'key))]
       [source-code (format-id #'key "~a-source-code" (syntax->datum #'key))]
       [source-file (format-id #'key "~a-source-file-path" (syntax->datum #'key))]
       [object-file (format-id #'key "~a-object-file-path" (syntax->datum #'key))]
       [compiler (syntax (make-parameter 'auto))]
       [compile-flags (syntax (make-parameter "-shared -Wl,-undefined,dynamic_lookup -fPIC -O2"))])
     #'(begin
         (define source-code (string-append code ...))
         (define source-file (get-cached-c-source-path source-code 'key))
         (define object-file (get-cached-c-obj-path source-code 'key))
         (unless (file-exists? source-file)
           (cache-inline-c source-code 'key))
         (unless (and (file-exists? source-file)
                      (file-exists? object-file)
                      (timestamp<? source-file object-file))
           (cache-compile-inline-c source-code #:key 'key #:compiler (compiler) #:compile-flags (compile-flags)))
         (define-dynamic-ffi/cached name (format "~a" source-file) source-file)))]))

