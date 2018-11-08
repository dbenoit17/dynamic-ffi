#lang racket/base

(require "dynamic-ffi-core.rkt")

(provide (rename-out
           [dynamic-ffi-wrapper dynamic-ffi-parse]))

(define (dynamic-ffi-wrapper . files-list)
  (define (path->byte-string path)
    (cond [(bytes? path) path]
          [(string? path) (string->bytes/locale path)]
          [(path? path)
             (string->bytes/locale
               (path->string path))]
          [else (error "dynamic-ffi: unsupported path format: " path)]))
  (define byte-string-paths
    (for/list ([path files-list])
      (path->byte-string path)))
  (apply dynamic-ffi-parse (cons #"dynamic-ffi-parse" byte-string-paths)))

(struct declaration [name type type-string])
(struct function-decl declaration [])
(struct var-decl declaration [])
(struct struct-decl declaration [])
(struct union-decl declaration [])

