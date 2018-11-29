#lang racket/base

(require
  racket/list
  "../dynamic-ffi/unsafe.rkt")


;; Builds an auto-ffi
(define-dynamic-ffi glfw.h "/usr/lib64/libglfw" "/usr/include/GLFW/glfw3.h")

(provide (all-defined-out))

