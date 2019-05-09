#lang racket/base

(require
  racket/list
  "../unsafe.rkt")

(provide (all-defined-out))


(define string1 "hello ")
(define string2 "world\n")

;; Builds an auto-ffi
(define-dynamic-ffi libc
  "/usr/lib64/libc-2.29"
  "/usr/include/string.h"
  "/usr/include/stdio.h"
  "/usr/include/stdio_ext.h")

(define (libc-strcat x y)
  (libc 'strcat x y))


(define (libc-printf s)
  ;; Does not work with varargs
  (libc 'printf s)
  (void))

;; Doesn't seem to be overwriting memory?
(define c (list->string (build-list 30 (lambda (x) #\c))))

(define x (libc-strcat string1 c))

(libc-printf "string1: ")
(libc-printf string1)
(libc-printf "\nstring2: ")
(libc-printf string2)
(libc-printf "\nc: ")
(libc-printf c)
(libc-printf "\nconcat: ")
(libc-printf x)
(libc-printf "\n")

(define (enum-ref-print enum)
  (printf "  ~a: ~a\n" enum (libc enum)))

(printf "\n enums constants: \n")
(enum-ref-print 'FSETLOCKING_BYCALLER)
(enum-ref-print  'FSETLOCKING_INTERNAL)
(enum-ref-print  'FSETLOCKING_QUERY)


