#lang racket/base

(require racket/contract/base)

(provide define-struct-expectation
         expect-struct
         (contract-out
          [struct (struct-accessor-context context)
            ([description string?] [accessor-id identifier?])
            #:omit-constructor]
          [make-struct-accessor-context
           (-> identifier? struct-accessor-context?)]))

(require (for-syntax racket/base
                     racket/sequence
                     racket/string
                     racket/syntax
                     syntax/parse/class/struct-id)
         syntax/parse/define
         "base.rkt"
         "combinator.rkt"
         "data/main.rkt"
         "logic.rkt")


(struct struct-accessor-context context (accessor-id) #:transparent)

(define (make-struct-accessor-context accessor-id)
  (define msg
    (format "the ~a struct field" (identifier-binding-symbol accessor-id)))
  (struct-accessor-context msg accessor-id))

(begin-for-syntax
  ;; for use in syntax-parse as: #:fail-when (check ...) "message"
  (define (check v error-stx) (and (not v) error-stx))

  (define (check-bound id-stx [error-stx id-stx])
    (check (identifier-binding id-stx) error-stx))

  (define (check-all-bound ids-stx) (ormap check-bound (syntax->list ids-stx)))

  (define (check-accessor id-stx info)
    (define known-accessors (list-ref info 3))
    (check (ormap (λ (known) (free-identifier=? id-stx known)) known-accessors)
           id-stx))

  (define (check-all-accessor ids-stx info)
    (ormap (λ (id) (check-accessor id info)) (syntax->list ids-stx))))

(define-simple-macro (expect-struct id:struct-id [accessor-id:id exp:expr] ...)
  #:fail-when (check (attribute id.predicate-id) #'id)
  "predicate for struct type not known"
  #:fail-when (check-bound (attribute id.predicate-id) #'id)
  (format "predicate ~a for struct type undefined"
          (identifier-binding-symbol #'id.predicate-id))
  #:fail-when (check-all-bound #'(accessor-id ...))
  "accessor undefined"
  #:fail-when (check-duplicate-identifier (syntax->list #'(accessor-id ...)))
  "duplicate accessor identifier"
  #:fail-when (check-all-accessor #'(accessor-id ...) (attribute id.info))
  (format "not known to be field accessor for struct ~a"
          (identifier-binding-symbol #'id))
  (expect-and (expect-pred id.predicate-id)
              (expect-all
               (expect/context (expect/proc (->expectation exp) accessor-id)
                               (make-struct-accessor-context #'accessor-id))
               ...)))

(begin-for-syntax
  (define (format-expect-id id-stx)
    (format-id id-stx "expect-~a" id-stx #:source id-stx))

  (define (accessor->keyword id-stx struct-id-stx)
    (define prefix (format "~a-" (identifier-binding-symbol struct-id-stx)))
    (define id-str (symbol->string (identifier-binding-symbol id-stx)))
    (define correct-format? (string-prefix? id-str prefix))
    (and correct-format?
         (string->keyword (substring id-str (string-length prefix)))))
  
  (define-syntax-class struct-id+expect
    #:attributes (id struct [struct.accessor-id 1])
    #:description "struct identifier or identifier and struct identifier pair"
    (pattern (id:id struct:struct-id))
    (pattern struct:struct-id #:with id (format-expect-id #'struct)))

  (define (syntax->list-of-lists stx) (map syntax->list (syntax->list stx))))

(define-simple-macro
  (define-struct-expectation id:struct-id+expect)
  #:with (accessor-id ...) #'(id.struct.accessor-id ...)
  #:do [(define keywords
          (map (λ (id) (accessor->keyword id #'id.struct))
               (syntax->list #'(accessor-id ...))))]
  #:fail-when (check (andmap values keywords) #'id.struct)
  (format "struct accessor ~a has ambiguous keyword form"
          (identifier-binding-symbol
           (for/first ([acc (in-syntax #'(accessor-id ...))]
                       [kw (in-syntax keywords)]
                       #:unless kw)
             acc)))
  #:with (keyword ...) keywords
  #:with (arg ...) (generate-temporaries #'(keyword ...))
  #:attr [formals 2] (syntax->list-of-lists #'((keyword [arg expect-any]) ...))
  (define (id.id formals ... ...)
    (expect-struct id.struct [accessor-id arg] ...)))
