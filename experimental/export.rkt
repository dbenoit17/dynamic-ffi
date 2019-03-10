#lang racket/base

(require
  ffi/unsafe
  racket/string
  racket/port
  "../runtime-paths.rkt"
  (prefix-in dffi: "../meta.rkt"))

(provide (all-defined-out)
         (all-from-out ffi/unsafe))

(define (format-list l)
  (format "(list ~a)"
    (string-join
      (for/list ([e l])
        (format "~a" e)))))

(define (format-string-list l)
  (format "(list ~a)"
    (string-join
      (for/list ([e l])
        (format "\"~a\"" e)))))

(define (format-ffi-int ct-int)
  (define width (dffi:ctype-width ct-int))
  (unless (and (modulo width 8) (<= width 64) (>= width 0))
    (error "incompatible int width: " width))
  (define ints  (vector '_int8 '_int16 '_int32 '_int64))
  (define uints  (vector '_uint8 '_uint16 '_uint32 '_uint64))
  (define category
    (if (dffi:ctype-int-signed? ct-int) ints uints))
  (define index (- (inexact->exact (log width 2)) 3))
  (vector-ref category index))

(define (format-ffi-float ct-float)
  (define width (dffi:ctype-width ct-float))
  (cond [(eq? width 32) '_float]
        [(eq? width 64) '_double]
        [(eq? width 80) '_longdouble]
        [(eq? width 128) '_longdouble]
        [else (error "incompatible float width: " width)]))

(define (format-ffi-pointer ct-pointer)
  (define pointee (dffi:ctype-pointer->pointee ct-pointer))
  (cond [(dffi:ctype-void? pointee) '_pointer]
        [(and (dffi:ctype-int? pointee)
              (eq? (dffi:ctype-width pointee) 8))
         '_string]
        ;; all pointers opaque for now to
        ;; prevent type conflicts
        [else '_pointer]))
        ;; would be cool to do it this way
        ;;[else (_cpointer (format-dffi-obj pointee))]))

(define (format-ffi-array ct-array)
  (define element (dffi:ctype-array-element ct-array))
  (format "(make-array-type ~a ~a)"
    (format-dffi-obj element)
      (quotient (dffi:ctype-width ct-array)
                (dffi:ctype-width element))))

(define (format-ffi-struct ct-struct)
  (define struct-members
    (for/list ([mem (dffi:ctype-record-members ct-struct)])
      (format-dffi-obj mem)))
  (if (null? struct-members) #f
  (format "(make-cstruct-type ~a)" struct-members)))

(define (format-ffi-union ct-union)
  (define union-members
    (for/list ([mem (dffi:ctype-record-members ct-union)])
      (format-dffi-obj mem)))
  (if (null? union-members) #f
    (format "(apply make-union-type ~a)" union-members)))

(define (format-ffi-function ct-function)
  (define params
   (for/list ([param (dffi:ctype-function-params ct-function)])
     (format-dffi-obj param)))
  (define maybe-return (dffi:ctype-function-return ct-function))
  (define return
    (if (dffi:ctype-void? maybe-return)
      '_void
      (format-dffi-obj maybe-return)))
  (format "(_cprocedure ~a ~a)" (format-list params) return))

(define (format-dffi-obj ct)
 (unless (dffi:ctype? ct)
  (error "expected dynamic-ffi ctype"))
 (cond
  [(dffi:ctype-int? ct) (format-ffi-int ct)]
  [(dffi:ctype-pointer? ct) (format-ffi-pointer ct)]
  [(dffi:ctype-float? ct) (format-ffi-float ct)]
  [(dffi:ctype-struct? ct) (format-ffi-struct ct)]
  [(dffi:ctype-union? ct) (format-ffi-union ct)]
  [(dffi:ctype-array? ct) (format-ffi-array ct)]
  [(dffi:ctype-function? ct) (format-ffi-function ct)]
  [(dffi:ctype-void? ct)
   (error "void only allowed as pointer or function return")]
  [else (error "unimplemented type")]))

(define (format-ffi-obj-map ffi-data lib . headers)
  (unless (for/or ([ext '(".so" ".dylib" ".dll")])
            (file-exists? (string-append lib ext)))
    (error "format-ffi: file does not exist: " (string-append lib ".so")))
  (define pairs
    (for/list ([decl ffi-data])
      (define name (dffi:declaration-name decl))
      (define type (dffi:declaration-type decl))
      (define ffi-obj
        (if (dffi:enum-decl? decl)
          (format "~a" (dffi:declaration-literal-value decl))
          (format "(get-ffi-obj '~a \"~a\"\n    ~a \n    (warn-undefined-symbol '~a))" name lib (format-dffi-obj type) name)))
      (cons (string->symbol name) ffi-obj)))
  (make-hash pairs))

(define (create-mapped-static-ffi ffi-data file ffi-name lib-path . headers)
  (define ffi-map
    (apply format-ffi-obj-map (cons ffi-data (cons lib-path headers))))
  (export-mapped-ffi file ffi-name lib-path headers ffi-map))

(define (create-static-ffi ffi-data file ffi-name lib-path . headers)
  (define ffi-map
    (apply format-ffi-obj-map (cons ffi-data (cons lib-path headers))))
  (export-ffi file ffi-name lib-path headers ffi-map))

(define (export-mapped-ffi file ffi-name lib-path headers ffi-map)
  (define template-port (open-input-file mapped-ffi-template-path))
  (define template (port->string template-port))
  (define formatted-pairs
    (format "(make-hash\n  ~a)"
     (format-list
       (for/list ([pr (filter (λ (x) (cdr x)) (hash->list ffi-map))])
         (format "\n   (cons '~a\n    ~a)" (car pr) (cdr pr))))))
  (close-input-port template-port)
  (with-output-to-file file #:exists 'replace
   (λ () (printf template
           ffi-name lib-path ffi-name (format-string-list headers)
           ffi-name ffi-name formatted-pairs ffi-name ffi-name
           ffi-name ffi-name))))

(define (export-ffi file ffi-name lib-path headers ffi-map)
  (define template-port (open-input-file defined-ffi-template-path))
  (define template (port->string template-port))
  (define formatted-definitions
     (string-join
       (for/list ([pr (filter (λ (x) (cdr x)) (hash->list ffi-map))])
         (format "(define ~a-~a\n  ~a)\n\n" ffi-name (car pr) (cdr pr)))
       ""))
  (close-input-port template-port)
  (with-output-to-file file #:exists 'replace
   (λ () (printf template
           ffi-name lib-path ffi-name (format-string-list headers)
           ffi-name formatted-definitions))))
