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

(define (timestamp<? i o)
  (< (file-or-directory-modify-seconds i)
     (file-or-directory-modify-seconds o)))

(define-syntax-rule (make-ffi-shared-lib out-path in ...)
 (let ([make-plugin-cmd
        (format-append cc "-v" in ... (get-llvm-config)
          lflags+= "-o" out-path cxxflags+= ldflags+=)])
   (unless (for/and ([i (list in ...)])
         (timestamp<? i out-path))
     (unless (system make-plugin-cmd)
       (error "header-parse: could not compile shared library")))
   out-path))

(define (make-native-libs)
  (when (for/and ([i (list dynamic-ffi.c header-parse.so)])
         (timestamp<? i dynamic-ffi-core_rkt.so))
    (void))
  (define cwd (current-directory))
  (current-directory shared-object-dir)
  (when (timestamp<? dynamic-ffi_3m.o dynamic-ffi.c)
    (printf "~a" dynamic-ffi.c)
    (system (format "raco ctool --xform ~a" dynamic-ffi.c))
    (printf "making object\n")
    (system (format "raco ctool --3m --cc ~a" dynamic-ffi.3m.c)))
  (printf "making extension\n")
  (system (format "raco ctool --3m --ld ~a ~a ~a"
            dynamic-ffi-core_rkt.so dynamic-ffi_3m.o header-parse.so))
  (current-directory cwd))

(define (post-installer x)
  (unless (directory-exists? dynamic-extension-dir)
    (make-directory* dynamic-extension-dir))
  (make-ffi-shared-lib ffi-plugin.so ffi-plugin.cc)
  (make-ffi-shared-lib header-parse.so header-parse.cc ffi-plugin.so)
  (make-native-libs))

(module+ main
  (post-installer #t))

