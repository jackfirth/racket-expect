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
         racket/list
         racket/set
         racket/vector
         syntax/parse/define
         "base.rkt"
         "compare.rkt"
         "data-hash.rkt"
         "data-list.rkt"
         "data-set.rkt"
         "data-vector.rkt"
         "function.rkt"
         "logic.rkt"
         "util.rkt")


(define expectation-convertible?
  (or/c expectation? list? vector? boolean? number? string? symbol? char?))

(define (expectation-convert v)
  (cond
    [(expectation? v) v]
    [(list? v) (apply expect-list (map expectation-convert v))]
    [(vector? v)
     (apply expect-vector (map expectation-convert (vector->list v)))]
    [(set? v) (apply expect-set (map expectation-convert (set->list v)))]
    [(boolean? v) (if v expect-true expect-false)]
    [((disjoin number? string? symbol? char?) v) (expect-equal? v)]))

(define (expect-equal?/convert v)
  (cond
    [(list? v) (apply expect-list (map expect-equal?/convert v))]
    [(vector? v)
     (apply expect-vector (map expect-equal?/convert (vector->list v)))]
    [(set? v) (apply expect-set (map expect-equal?/convert (set->list v)))]
    [else (expect-equal? v)]))

(define (expect-not-equal?/convert v)
  (cond
    [(list? v) (apply expect-list (map expect-not-equal?/convert v))]
    [(vector? v)
     (apply expect-vector (map expect-not-equal?/convert (vector->list v)))]
    [(set? v) (apply expect-set (map expect-not-equal?/convert (set->list v)))]
    [else (expect-not-equal? v)]))

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

(define-simple-macro (define-conversions [header contract] ...)
  (begin (define/expectation-conversion header contract) ...))

(define exp? expectation?)
(define cvrt? expectation-convertible?)

(define (expect-hash/convert . k+vs)
  (define converted (map expectation-convert (slice k+vs #:start 1 #:step 2)))
  (apply expect-hash (append-map list (slice k+vs #:step 2) converted)))

(provide
 (contract-out
  [rename expect-hash/convert expect-hash (rest->* (list any/c cvrt?) exp?)]))

(define-conversions
  [(expect-all . <convert>) (rest-> cvrt? exp?)]
  [(expect-and . <convert>) (rest-> cvrt? exp?)]
  [(expect-hash-count <convert>) (-> cvrt? exp?)]
  [(expect-hash-ref k <covert>) (-> any/c cvrt? exp?)]
  [(expect-hash-keys <convert>) (-> cvrt? exp?)]
  [(expect-list . <convert>) (rest-> cvrt? exp?)]
  [(expect-list-ref <convert> v) (-> cvrt? exact-nonnegative-integer? exp?)]
  [(expect-list-count <convert>) (-> cvrt? exp?)]
  [(expect-set-count <convert>) (-> cvrt? exp?)]
  [(expect-vector . <convert>) (rest-> cvrt? exp?)]
  [(expect-vector-ref <convert> v) (-> cvrt? exact-nonnegative-integer? exp?)]
  [(expect-vector-count <convert>) (-> cvrt? exp?)]
  [(expect-raise <convert>) (-> cvrt? exp?)]
  [(expect-return <convert>) (-> cvrt? exp?)])
