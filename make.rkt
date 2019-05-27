#lang racket/base

(require racket/system
         racket/file
         racket/port
         racket/string
         racket/runtime-path
         racket/set
         file/glob
         "runtime-paths.rkt"
         "common.rkt")

(provide (all-defined-out))

(define cc "clang++")
(define ldflags+= "-shared -Wl,-undefined,dynamic_lookup")
(define cflags+= "-fPIC")
(define cxxflags+= "-fno-rtti -O3")
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

(define clang-include
 (list
  "clang/AST/AST.h"
  "clang/AST/ASTConsumer.h"
  "clang/AST/RecursiveASTVisitor.h"
  "clang/Frontend/FrontendPluginRegistry.h"
  "clang/Frontend/CompilerInstance.h"
  "clang/Frontend/FrontendActions.h"
  "llvm/Support/raw_ostream.h"
  "clang/Frontend/FrontendActions.h"
  "clang/Tooling/CommonOptionsParser.h"
  "clang/Tooling/Tooling.h"))


(define (clang-headers-exist?)
  (unless (find-executable-path "llvm-config")
    #f)
  (define llvm-include-dir
    (string-trim
      (with-output-to-string
        (λ () (system "llvm-config --includedir")))))
  (printf "llvm include dir: ~a\n" llvm-include-dir)
  (for/and  ([h clang-include])
    (printf "~a\n" (build-path llvm-include-dir h))))

(define (libclang-exists?)
  (system "ldconfig -p | grep -q libclang"))

(define (clang++-exists?)
  (find-executable-path "clang++"))
    

;; Like string join but works with any
;; formattable type
(define (format-append . a)
 (define fmt
  (string-join
   (build-list (length a) (λ (x) "~a"))))
 (apply format (cons fmt a)))


(define-syntax-rule (make-ffi-shared-lib out-path in ...)
 (let ([make-plugin-cmd
        (format-append cc "-v" in ... (get-llvm-config)
          lflags+= "-o" out-path cflags+= cxxflags+= ldflags+=)])
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
  (current-directory cwd))

(define (raco-link)
  (define ctool-ld
    (format "raco ctool --3m --ld ~a ~a ~a"
            dynamic-ffi-core_rkt.so dynamic-ffi_3m.o clang-export.so))
  (printf "making extension\n~a\n" ctool-ld)
  (system ctool-ld))

(define (make-dynamic-ffi-pkg)
  (make-ffi-shared-lib wrap-fork.so wrap-fork.c)
  (make-ffi-shared-lib clang-plugin.so clang-plugin.cc)
  (make-ffi-shared-lib clang-export.so clang-export.c clang-plugin.so wrap-fork.so)
  (make-native-libs))

(define (make-native-dirs)
  (unless (directory-exists? compiled-dir)
    (make-directory* compiled-dir))
  (unless (directory-exists? dynamic-extension-dir)
    (make-directory* dynamic-extension-dir))
  (unless (directory-exists? compiled-native-dir)
    (system (format "ln -s ~a ~a" shared-object-dir compiled-native-dir))))

(define (post-installer x)
  (cond [(and (clang++-exists?) 
              (clang-headers-exist?)
              (libclang-exists?))
    (and 
      (make-native-dirs)
      (make-dynamic-ffi-pkg)
      (raco-link))]
    [else
      (warn-dependencies)
      #f]))

(module+ main
  (post-installer #t))

