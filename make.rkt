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
      (λ () (system "llvm-config --cxxflags --ldflags"))))
  (string-replace llvm-config "\n" " "))


;; Like string join but works with any
;; formattable type
(define (format-append . a)
  (define fmt 
    (string-join 
      (build-list (length a) (λ (x) "~a"))))
  (apply format (cons fmt a)))

(define (make-plugin plugin-src out-path)
  (define make-plugin-cmd
    (format-append cc "-v" plugin-src (get-llvm-config) 
                   "-o" out-path cxxflags+= ldflags+=))
  (system make-plugin-cmd))

(define (parse-header header)
  (define ffi-plugin.so (make-temporary-file "ffi-~a.so"))
  (define fmt 
    (string-append "clang -std=c99 -c ~a -Xclang -load -Xclang ~a "
                   "-Xclang -plugin -Xclang dynamic-ffi" ))
  (define clang-parse-cmd (format fmt header ffi-plugin.so))
  (unless (make-plugin ffi-plugin-src ffi-plugin.so)
    (error "plugin failed to compile"))
  (define output
    (with-output-to-string
      (λ ()
        (system clang-parse-cmd))))
  output)
        
  
