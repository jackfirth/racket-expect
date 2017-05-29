#lang racket/base

(require racket/contract)

(provide expect-eq?
         (contract-out
          [expectation-convert (-> expectation-convertible? expectation?)]
          [expectation-convertible? predicate/c]
          [rename expect-equal?/convert expect-equal?
                  (-> any/c expectation?)]
          [rename expect-not-equal?/convert expect-not-equal?
                  (-> any/c expectation?)]))

(require (for-syntax racket/base
                     racket/syntax)
         racket/function
         syntax/parse/define
         "base.rkt"
         "combinator.rkt"
         "data.rkt")


(define expectation-convertible?
  (or/c expectation? list? boolean? number? string? symbol? char?))

(define (expectation-convert v)
  (cond
    [(expectation? v) v]
    [(list? v) (apply expect-list (map expectation-convert v))]
    [(boolean? v) (if v expect-true expect-false)]
    [((disjoin number? string? symbol? char?) v) (expect-equal? v)]))

(define-syntax <convert> #f)

(begin-for-syntax
  (define-syntax-class arg-id
    #:literals (<convert>)
    (pattern <convert>
             #:attr id (generate-temporary)
             #:attr expr #'(expectation-convert id))
    (pattern id:id #:attr expr #'id)))

(define-syntax-parser define/expectation-conversion
  [(_ (id:id arg:arg-id ... . rest-arg:arg-id) contract-expr:expr)
   #'(begin
       (define (converted arg.id ... . rest-arg.id)
         (apply id arg.expr ... (map expectation-convert rest-arg.id)))
       (provide (contract-out (rename converted id contract-expr))))]
  [(_ (id:id arg:arg-id ...) contract-expr:expr)
   #'(begin
       (define (converted arg.id ...) (id arg.expr ...))
       (provide (contract-out (rename converted id contract-expr))))])

(define (expectation-convert/equal? v)
  (cond
    [(list? v) (apply expect-list (map expectation-convert/equal? v))]
    [else v]))

(define (expect-equal?/convert v)
  (cond
    [(list? v) (apply expect-list (map expect-equal?/convert v))]
    [else (expect-equal? v)]))

(define (expect-not-equal?/convert v)
  (cond
    [(list? v) (apply expect-list (map expect-not-equal?/convert v))]
    [else (expect-not-equal? v)]))

(define/expectation-conversion (expect-list . <convert>)
  (->* () #:rest (listof expectation-convertible?) expectation?))