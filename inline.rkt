;; This module provides a macro for defining
;; inline FFIs.

#lang racket/base

(require openssl/sha1
         racket/system
         racket/runtime-path
         (for-syntax
           racket/base
           racket/syntax
           syntax/parse)
         "cached.rkt")

(provide define-inline-ffi)

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
    (error "could not find compiler on path: " compiler))
  (define object-file (get-cached-c-obj-path c-code key))
  (define cmd (format "~a ~a -o ~a ~a" compiler source object-file flags))
  (system cmd)
  (void))

;; I've been worried recently of the potential
;; for unwitting programmers to introduce
;; arbitrary execution vulnerabilities into
;; programs by injecting unsanitized strings
;; into their inline C code.
;;
;; Thus, the library will now reject non-literal strings
;; that are passed to define-inline-ffi by default.  I
;; realize this drastically reduces the things you can
;; do with inline ffi definitions, like code generation.
;; However I think the security implications outweigh
;; flexibility enough to forbid non-literals by default.
;;
;; I strongly advise against removing the string-literal check.
(define-syntax (define-inline-ffi stx)
  (syntax-parse stx
    [(_ key:id
       (~optional (~seq #:compile-flags flags) #:defaults ([flags #'""]))
       (~optional (~seq #:compiler compiler) #:defaults ([compiler #''auto]))
       ;; Do not document this keyword argument in the scribble
       ;; documentation. It can be useful in circumstances like
       ;; compiler research but is terribly dangerous if not
       ;; treated carefully.  Definitely do not use this flag
       ;; in production.  If you choose to ignore all the warnings
       ;; spattered throughout the library, please be aware that you
       ;; do so at your own great risk.
       (~optional
         (~seq #:WARNING-dangerously-insecure-do-not-use
               accept-dangerous-non-literal-strings?)
         #:defaults ([accept-dangerous-non-literal-strings? #'#f]))
        code ...)
     #:declare code (expr/c #'string?)
     #:declare compiler (expr/c #'string?)
     #:declare flags (expr/c #'string?)
     #:declare accept-dangerous-non-literal-strings? (expr/c #'boolean?)
     #:fail-when
       ;; Assert literal strings only.  Just short
       ;; circuit if the check is disabled.
       (not (or (syntax-e #'accept-dangerous-non-literal-strings?)
                (for/and ([str (syntax-e #'(code ...))])
                  (string? (syntax-e str)))))
       (string-append
         "Expected string literal."
         "\n Note: define-inline-ffi forbids the use of"
         "\n       non-literal strings to help discourage "
         "\n       users from introducing arbitrary code"
         "\n       execution vulnerabilities into programs.")
     (with-syntax*
       ([name  (format-id #'key "~a" (syntax->datum #'key))]
       [source-code (format-id #'key "~a-source-code" (syntax->datum #'key))]
       [source-file (format-id #'key "~a-source-file-path" (syntax->datum #'key))]
       [object-file (format-id #'key "~a-object-file-path" (syntax->datum #'key))]
       [compile-flags
        (syntax (format "~a ~a" default-flags flags))])
     #'(begin
         (define source-code (string-append code.c ...))
         (define source-file (get-cached-c-source-path source-code 'key))
         (define object-file (get-cached-c-obj-path source-code 'key))
         (unless (file-exists? source-file)
           (cache-inline-c source-code 'key))
         (unless (and (file-exists? source-file)
                      (file-exists? object-file)
                   (timestamp<=? source-file object-file))
           (cache-compile-inline-c source-code  #:key 'key
                                   #:compiler compiler
                                   #:compile-flags compile-flags))
         (define-dynamic-ffi/cached name (format "~a" source-file) source-file)))]))

