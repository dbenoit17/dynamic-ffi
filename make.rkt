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
(define cxxflags+= "-fno-rtti -fPIC")
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
   (unless
     (and (file-exists? out-path)
       (for/and ([i (list in ...)])
         (and (file-exists? i)
           (timestamp<? i out-path))))
     (unless (system make-plugin-cmd)
       (error "clang-export: could not compile shared library")))
   out-path))

(define (make-native-libs)
  (when (and (file-exists? dynamic-ffi-core_rkt.so)
          (for/and ([i (list dynamic-ffi.c clang-export.so)])
              (file-exists? i)
              (timestamp<? i dynamic-ffi-core_rkt.so)))
    (void))
  (define cwd (current-directory))
  (current-directory shared-object-dir)
  (when
    (or (not (file-exists? dynamic-ffi_3m.o)) (timestamp<? dynamic-ffi_3m.o dynamic-ffi.c))
    (define ctool-xform (format "raco ctool --xform ~a" dynamic-ffi.c))
    (define ctool-3m (format "raco ctool --3m --cc ~a" dynamic-ffi.3m.c))
    (printf "~a\n" ctool-xform)
    (system ctool-xform)
    (printf "making object\n~a\n" ctool-3m)
    (system ctool-3m))
  (define ctool-ld
    (format "raco ctool --3m --ld ~a ~a ~a"
            dynamic-ffi-core_rkt.so dynamic-ffi_3m.o clang-export.so))
  (printf "making extension\n~a\n" ctool-ld)
  (system ctool-ld)
  (current-directory cwd))

(define (pre-installer x)
  (unless (directory-exists? dynamic-extension-dir)
    (make-directory* dynamic-extension-dir))
  (make-ffi-shared-lib clang-plugin.so clang-plugin.cc)
  (make-ffi-shared-lib clang-export.so clang-export.cc clang-plugin.so)
  (make-native-libs))

(module+ main
  (pre-installer #t))

