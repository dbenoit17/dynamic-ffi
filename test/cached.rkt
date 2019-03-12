#lang racket/base

;(require "../unsafe.rkt")
(require "../experimental/cached.rkt")

(define-dynamic-ffi/cached libc
  "/usr/lib64/libc-2.28"
  "/usr/include/stdio.h")

(libc 'printf "hello world\n")


