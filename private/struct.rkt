#lang racket/base

(require racket/contract/base)

(provide define-struct-expectation
         expect-struct
         (contract-out
          [struct (struct-field-context context)
            ([description string?]
             [type-id identifier?]
             [field-id identifier?])
            #:omit-constructor]
          [make-struct-field-context
           (-> identifier? identifier? struct-field-context?)]))

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/class/struct-id)
         syntax/parse/define
         "base.rkt"
         "combinator.rkt"
         "data/main.rkt"
         "logic.rkt")


(struct struct-field-context context (type-id field-id) #:transparent)

(define (make-struct-field-context type-id field-id)
  (struct-field-context (format "the ~a-~a struct field"
                                (syntax->datum type-id)
                                (syntax->datum field-id))
                        type-id
                        field-id))

(begin-for-syntax
  (define (not-in-fields? known-accessors field-id accessor-id)
    (define in-fields?
      (ormap (λ (known) (free-identifier=? accessor-id known))
             known-accessors))
    (and (not in-fields?) field-id))
  
  (define (check-struct-field-ids struct-info field-ids accessor-ids)
    (define known-accessors (list-ref struct-info 3))
    (ormap (λ (f a) (not-in-fields? known-accessors f a))
           field-ids
           accessor-ids))

  (define (field->accessor field-id struct-id)
    (format-id field-id "~a-~a" struct-id (syntax->datum field-id)
               #:source field-id)))

(define-simple-macro (expect-struct id:struct-id [field-id:id exp:expr] ...)
  #:fail-when (and (not (attribute id.predicate-id)) #'id)
  "predicate for struct type not known"
  #:fail-when (and (not (identifier-binding (attribute id.predicate-id))) #'id)
  "predicate for struct type not bound"
  #:fail-when (and (not (attribute id.all-fields-visible?)) #'id)
  "fields for struct type not all visible"
  #:do [(define fields (syntax->list #'(field-id ...)))]
  #:fail-when (check-duplicate-identifier fields) "duplicate field identifier"
  #:with (accessor ...) (map (λ (f) (field->accessor f #'id)) fields)
  #:fail-when (check-struct-field-ids
               (attribute id.info) fields (syntax->list #'(accessor ...)))
  (format "field not a member of struct type ~a" (syntax->datum #'id))
  (expect-and (expect-pred id.predicate-id)
              (expect-all
               (expect/context (expect/proc (->expectation exp) accessor)
                               (make-struct-field-context #'id #'field-id))
               ...)))

(begin-for-syntax
  (define (format-expect-id id-stx)
    (format-id id-stx "expect-~a" id-stx #:source id-stx))
  
  (define-syntax-class struct-id+expect
    #:description "identifier or identifier pair"
    (pattern (id:id struct:struct-id))
    (pattern struct:struct-id #:attr id (format-expect-id #'struct)))
  
  (define (id->keyword id-stx)
    (string->keyword (symbol->string (syntax->datum id-stx))))
  
  (define-syntax-class field-id+kw
    #:description "identifier or identifier and keyword"
    #:attributes (id kw [formals 1])
    (pattern (id:id kw:keyword)
             #:attr [formals 1] (list #'kw #'[id expect-any]))
    (pattern id:id #:attr kw #`#,(id->keyword #'id)
             #:attr [formals 1] (list #'kw #'[id expect-any]))))

(define-simple-macro
  (define-struct-expectation id:struct-id+expect (id+kw:field-id+kw ...))
  (define (id.id id+kw.formals ... ...)
    (expect-struct id.struct [id+kw.id id+kw.id] ...)))
