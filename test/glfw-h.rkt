#lang racket/base

(require
  racket/list
  "../unsafe.rkt")


;; Builds an auto-ffi
(define-dynamic-ffi glfw.h "/usr/lib64/libglfw" "/usr/include/GLFW/glfw3.h")

(provide (all-defined-out))

