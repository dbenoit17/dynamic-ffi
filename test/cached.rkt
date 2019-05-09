#lang racket/base

;(require "../unsafe.rkt")
(require "../unsafe.rkt")

(define-dynamic-ffi libc
  "/usr/lib64/libc-2.29"
  "/usr/include/stdio.h")

(libc 'printf "hello world\n")


