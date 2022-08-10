;; This module provides functions for exporting
;; FFI bindings as shippable racket files.

#lang racket/base

(require
  racket/string
  racket/port
  racket/runtime-path
  racket/contract
  racket/pretty
  racket/match
  (only-in ffi/unsafe ffi-lib _byte get-ffi-obj)
  (for-syntax racket/base)
  (prefix-in dffi: "meta.rkt"))

(provide 
  create-mapped-static-ffi
  create-static-ffi
  (contract-out          
    [generate-static-ffi
     (->* ((or/c string? symbol?)
           (or/c string? path?)
           (or/c string? path? (listof string?)))
          (#:prune-undefined? any/c
           #:named-structs? any/c
           #:prefix-definitions? any/c)
          #:rest (listof (or/c string? path?))
          any)]
    [generate-mapped-static-ffi
     (->* ((or/c string? symbol?)
           (or/c string? path?)
           (or/c string? path? (listof string?)))
          (#:prune-undefined? any/c)
          #:rest (listof (or/c string? path?))
          any)]))

(define-runtime-path mapped-ffi-template-path
  (build-path "template-files" "mapped-ffi-template"))
(define-runtime-path defined-ffi-template-path
  (build-path "template-files" "defined-ffi-template"))

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
        [(eq? width 64) '_double*]
        [(eq? width 80) '_longdouble]
        [(eq? width 128) '_longdouble]
        [else (error "incompatible float width: " width)]))

(define (format-ffi-pointer ct-pointer)
  (define pointee (dffi:ctype-pointer->pointee ct-pointer))
  (cond [(dffi:ctype-void? pointee) '_pointer]
        [(and (dffi:ctype-int? pointee)
              (eq? (dffi:ctype-width pointee) 8))
         '_string]
        [(dffi:ctype-function? pointee)
         (format "(_cpointer ~a)" (format-dffi-obj pointee))]
        ;; all pointers opaque for now to
        ;; prevent type conflicts
        [else '_pointer]))
        ;; would be cool to do it this way
        ;;[else (format "(_cpointer ~a)" (format-dffi-obj pointee))]))

(define (format-ffi-array ct-array)
  (define element (dffi:ctype-array-element ct-array))
  (format "(make-array-type ~a ~a)"
    (format-dffi-obj element)
      (quotient (dffi:ctype-width ct-array)
                (dffi:ctype-width element))))

(define (format-ffi-struct ct-struct #:named? [named? #f])
  (define struct-members
    (for/list ([mem (dffi:ctype-record-members ct-struct)])
      (format-dffi-obj mem)))
  (define member-names (dffi:ctype-record-member-names ct-struct))
  (cond [(null? struct-members) #f]
        [named? `(cstruct ,(pretty-format (for/list ([name member-names]
                                                     [mem struct-members])
                                            `[,name ,mem])
                                          #:mode 'display))]
        [else (format "(apply _list-struct ~a)" (format-list struct-members))]))

(define (format-ffi-union ct-union)
  (define union-members
    (for/list ([mem (dffi:ctype-record-members ct-union)])
      (format-dffi-obj mem)))
  (if (null? union-members) #f
    (format "(apply _union ~a)" (format-list union-members))))

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

(define (format-dffi-obj ct
                         #:named-structs? [named-structs? #f])
 (unless (dffi:ctype? ct)
  (error "expected dynamic-ffi ctype"))
 (cond
  [(dffi:ctype-int? ct) (format-ffi-int ct)]
  [(dffi:ctype-pointer? ct) (format-ffi-pointer ct)]
  [(dffi:ctype-float? ct) (format-ffi-float ct)]
  [(dffi:ctype-struct? ct) (format-ffi-struct ct #:named? named-structs?)]
  [(dffi:ctype-union? ct) (format-ffi-union ct)]
  [(dffi:ctype-array? ct) (format-ffi-array ct)]
  [(dffi:ctype-function? ct) (format-ffi-function ct)]
  [(dffi:ctype-void? ct)
   (error "void only allowed as pointer or function return")]
  [else (error "unimplemented type")]))

;; This is equivalent to build-ffi-obj-map in ffi.rkt,
;; except it produces racket source code instead
;; of runtime ffi objects.
;; ffi-lib-obj is either #f or a foreign-library value where #f means
;; don't check whether a symbol is defined.
(define (format-ffi-obj-map ffi-data lib ffi-lib-obj
                            #:named-structs? [named-structs? #f]
                            . headers)
  (define pairs
    (for/list ([decl ffi-data])
      (define name (dffi:declaration-name decl))
      (define type (dffi:declaration-type decl))
      (define ffi-obj
        (cond [(dffi:enum-decl? decl)
               (format "~a" (dffi:declaration-literal-value decl))]
              [(or (dffi:record-decl? decl)
                   (dffi:typedef-decl? decl))
               (format-dffi-obj type #:named-structs? named-structs?)]
              [(or (dffi:function-decl? decl)
                   (dffi:var-decl? decl))
               (cond
                 [(or (not ffi-lib-obj)
                      (get-ffi-obj (string->symbol name) ffi-lib-obj _byte (λ () #f)))
                  ;; NOTE: use _byte as a dummy type which will return a Racket integer
                  ;; when the symbol exists, #f otherwise
                  (format "(get-ffi-obj '~a ~a\n    ~a \n    (warn-undefined-symbol '~a))"
                          name lib (format-dffi-obj type) name)]
                 [else
                  (eprintf "warning: ~a is undefined\n" name)
                  #f])]
              [else
               (eprintf "warning: unimplemented delcaration type: ~a\n" decl)
               #f]))
      (cons (string->symbol name) ffi-obj)))
  (make-hash (filter (λ (x) (cdr x)) pairs)))


;; The following functions feel ambiguously named to me, so I'll
;; document their exact usage in more detail.

;; The export ffi functions are the core exporting functions
;; They take a formatted ffi map produced by format-ffi-obj-map
;; and export an ffi to an output file
(define (export-mapped-ffi file ffi-name library-name lib headers ffi-map)
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
           library-name lib ffi-name (format-string-list headers)
           ffi-name formatted-pairs ffi-name))))

(define ((export-ffi prefix-definitions?) file ffi-name library-name lib headers ffi-map)
  (define template-port (open-input-file defined-ffi-template-path))
  (define template (port->string template-port))
  (define formatted-definitions
     (string-join
       (for/list ([pr (filter (λ (x) (cdr x)) (hash->list ffi-map))])
         (define name (if prefix-definitions? (format "~a-~a" ffi-name (car pr)) (car pr)))
         (match (cdr pr)
           [`(cstruct ,rst)
            (format "(define-cstruct _~a\n  ~a)\n\n" name rst)]
           [_ (format "(define ~a\n  ~a)\n\n" name (cdr pr))]))
       ""))
  (close-input-port template-port)
  (with-output-to-file file #:exists 'replace
   (λ () (printf template
           library-name lib ffi-name (format-string-list headers)
           ffi-name formatted-definitions))))

;; The create ffi functions take unformatted ffi metadata produced by
;; dffi:dynamic-ffi-parse, format the data for export, and wrap the export
;; ffi functions.  These are useful when ffi-data is generated earlier
;; in a routine and used by other functions than just export.
;; define-dynamic-ffi/cached in cached.rkt is an example of this.
(define (create-static-ffi-generic dispatch ffi-data prune-undefined?
                                   file ffi-name lib headers
                                   #:named-structs? [named-structs? #f])
  (define library-name (format "~a-ffi-lib" ffi-name))
  (define-values (ffi-library ffi-lib-obj)
    (cond [(or (string? lib) (path? lib))
           (values (format "(ffi-lib \"~a\")" lib)
                   (and prune-undefined? (ffi-lib lib)))]
          [(pair? lib)
           (values (format "(ffi-lib \"~a\" ~a)" (car lib)  (format-string-list (cdr lib)))
                   (and prune-undefined? (ffi-lib (car lib) (cdr lib))))]))
  (define ffi-map (apply format-ffi-obj-map ffi-data library-name ffi-lib-obj
                         #:named-structs? named-structs?
                         headers))
  (dispatch file ffi-name library-name ffi-library headers ffi-map))

(define (create-mapped-static-ffi #:prune-undefined? [prune-undefined? #f]
                                  ffi-data file ffi-name lib . headers)
  (create-static-ffi-generic export-mapped-ffi ffi-data prune-undefined?
                             file ffi-name lib headers))

(define (create-static-ffi #:prune-undefined? [prune-undefined? #f]
                           #:prefix-definitions? [prefix-definitions? #t]
                           #:named-structs? [named-structs? #f]
                           ffi-data file ffi-name lib . headers)
  (create-static-ffi-generic (export-ffi prefix-definitions?) ffi-data
                             prune-undefined? file ffi-name lib headers
                             #:named-structs? named-structs?))

;; The generate ffi functions are the only user-facing export functions
;; provided by dynamic-ffi/unsafe.  These functions take only the
;; desired ffi name, the lib file, and the header files, and invoke
;; dffi:dynamic-ffi-parse themselves to export a static ffi.
(define (generate-mapped-static-ffi #:prune-undefined? [prune-undefined? #f]
                                    ffi-name file lib-path . headers)
  (define ffi-data (apply dffi:dynamic-ffi-parse headers))
  (apply create-mapped-static-ffi ffi-data file ffi-name lib-path headers
         #:prune-undefined? prune-undefined?))

(define (generate-static-ffi #:prune-undefined? [prune-undefined? #f]
                             #:prefix-definitions? [prefix-definitions? #t]
                             #:named-structs? [named-structs? #f]
                             ffi-name file lib-path . headers)
  (define ffi-data (apply dffi:dynamic-ffi-parse headers))
  (apply create-static-ffi ffi-data file ffi-name lib-path headers
         #:prefix-definitions? prefix-definitions?
         #:named-structs? named-structs?
         #:prune-undefined? prune-undefined?))
