#lang racket/base

(require racket/system
         racket/file
         racket/port
         racket/string
         racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path ffi-plugin-src "clang/dynamic-ffi-plugin.cc")

(define cc "clang++")
(define ldflags+= "-shared -Wl,-undefined,dynamic_lookup")
(define cxxflags+= "-fno-rtti")

(define (get-llvm-config)
  (define llvm-config
    (with-output-to-string
      (lambda () (system "llvm-config --cxxflags --ldflags"))))
  (string-replace llvm-config "\n" " "))

(define (format-append . a)
  (define fmt 
    (string-join 
      (build-list (length a) (lambda (x) "~a"))))
  (apply format (cons fmt a)))

(define (make-plugin plugin-src out-path)
  (define make-plugin-cmd
    (format-append cc "-v" plugin-src (get-llvm-config) 
                   "-o" out-path cxxflags+= ldflags+=))
  (system make-plugin-cmd))
        
  
