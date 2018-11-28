#lang racket/base

(require
  racket/list
  "../dynamic-ffi/unsafe.rkt")


(define string1 "hello")
(define string2 " world")
(define string1+string2
  (string-append string1 string2))


;; Builds an auto-ffi
(define-dynamic-ffi string.h "/usr/include/string.h" "/usr/lib64/libc-2.28")

;; Call strcat from built ffi
(define strcat-result (string.h-funcall 'strcat string1 string2))


(printf "string1+string2\n  \"~a\"\n" string1+string2)
(printf "(string.h-funcall 'strcat string1 string2)\n  \"~a\"\n" strcat-result)
(printf "strings equal?: ~a\n"
  (string=? string1+string2 strcat-result))

;; Output

; parse complete
; string1+string2
;   "hello world"
; (string.h-funcall 'strcat string1 string2)
;   "hello world"
; strings equal?: #t


