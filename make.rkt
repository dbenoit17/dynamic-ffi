#lang racket/base

(require racket/system
         racket/file
         racket/port
         racket/string
         racket/runtime-path
         "runtime-paths.rkt")

(provide (all-defined-out))

(define cc "clang++")
(define ldflags+= "-shared -Wl,-undefined,dynamic_lookup")
(define cxxflags+= "-fno-rtti")
(define lflags+=
 (string-join
  (list "-lclangAST"
   " -lclangASTMatchers"
   " -lclangAnalysis"
   " -lclangBasic"
   " -lclangDriver"
   " -lclangEdit"
   " -lclangFrontend"
   " -lclangFrontendTool"
   " -lclangLex"
   " -lclangParse"
   " -lclangSema"
   " -lclangEdit"
   " -lclangRewrite"
   " -lclangRewriteFrontend"
   " -lclangStaticAnalyzerFrontend"
   " -lclangStaticAnalyzerCheckers"
   " -lclangStaticAnalyzerCore"
   " -lclangSerialization"
   " -lclangToolingCore"
   " -lclangTooling"
   " -lclangFormat"
   " -lLLVM")))

(define (get-llvm-config)
 (define llvm-config
  (with-output-to-string
   (λ () (system "llvm-config --cxxflags --ldflags --libs --libfiles --system-libs"))))
 (string-replace llvm-config "\n" " "))


;; Like string join but works with any
;; formattable type
(define (format-append . a)
 (define fmt
  (string-join
   (build-list (length a) (λ (x) "~a"))))
 (apply format (cons fmt a)))

(define (make-ffi-shared-lib plugin-src out-path)
 (define make-plugin-cmd
  (format-append cc "-v" plugin-src (get-llvm-config)
   lflags+= "-o" out-path cxxflags+= ldflags+=))
 (unless (system make-plugin-cmd)
   (error "dynamic-ffi: could not compile shared library"))
 out-path)

(define (post-installer x)
  (make-ffi-shared-lib dynamic-ffi.cc dynamic-ffi.so))

