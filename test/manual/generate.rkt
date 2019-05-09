#lang racket/base

(require dynamic-ffi/unsafe)


(generate-mapped-static-ffi 'libc 
 "libc.mapped-ffi.rkt" 
 (dynamic-ffi-lib "libc" "6")
 "/usr/include/string.h")

(generate-static-ffi 'libc 
  "libc.ffi.rkt" 
 (dynamic-ffi-lib "libc" "6")
  "/usr/include/string.h")