#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-compare (-> compare/c any/c expectation?)]
  [expect-not-compare (-> compare/c any/c expectation?)]
  [expect-eq? (-> any/c expectation?)]
  [expect-not-eq? (-> any/c expectation?)]
  [expect-eqv? (-> any/c expectation?)]
  [expect-not-eqv? (-> any/c expectation?)]
  [expect-not-equal? (-> any/c expectation?)]
  [expect-= (-> real? real? expectation?)]
  [struct (compare-attribute attribute)
    ([description string?] [proc compare/c] [other any/c])
    #:omit-constructor]
  [make-compare-attribute (-> compare/c any/c compare-attribute?)]
  [struct (=-attribute attribute)
    ([description string?] [value real?] [epsilon real?]) #:omit-constructor]
  [make-=-attribute (-> real? real? =-attribute?)]))

(require syntax/parse/define
         "base.rkt"
         "combinator.rkt"
         "logic.rkt")


(define compare/c (-> any/c any/c any/c))

(struct compare-attribute attribute (proc other) #:transparent)
(define (make-compare-attribute proc other)
  (define desc (format "~a when compared to ~v" (object-name proc) other))
  (compare-attribute desc proc other))

(define (expect-compare compare other)
  (define (make-fault v)
    (and (not (compare v other))
         (fault #:summary "a different value"
                #:expected (make-compare-attribute compare other)
                #:actual (make-self-attribute v))))
  (expectation-rename (expect/singular make-fault) (object-name compare)))

(define (expect-not-compare compare other)
  (define attr (make-not-attribute (make-compare-attribute compare other)))
  (define (make-fault v)
    (and (compare v other)
         (fault #:summary "a different value"
                #:expected attr
                #:actual (make-self-attribute v))))
  (define name (string->symbol (format "not-~a" (object-name compare))))
  (expectation-rename (expect/singular make-fault) name))

(define (expect-eq? v) (expect-compare eq? v))
(define (expect-eqv? v) (expect-compare eqv? v))
(define (expect-not-eq? v) (expect-not-compare eq? v))
(define (expect-not-eqv? v) (expect-not-compare eqv? v))
(define (expect-not-equal? v) (expect-not-compare equal? v))

(define-simple-macro (define-attr-sugar [id:id pred:id proc:id] ...+)
  (begin
    (begin
      (define (id v) (make-compare-attribute proc v))
      (define (pred v)
        (and (compare-attribute? v) (equal? (compare-attribute-proc v) proc)))
      (provide
       (contract-out [id (-> any/c compare-attribute?)] [pred predicate/c])))
    ...))

(define-attr-sugar
  [make-eq-attribute eq-attribute? eq?]
  [make-eqv-attribute eqv-attribute? eqv?]
  [make-equal-attribute equal-attribute? equal?])

(struct =-attribute attribute (value epsilon)
  #:transparent)

(define (make-=-attribute value tolerance)
  (define desc (format "= to ~v (with tolerance of ~v)" value tolerance))
  (=-attribute desc value tolerance))

(define (expect-= e tolerance)
  (define lower (- e tolerance))
  (define upper (+ e tolerance))
  (define (make-fault v)
    (and (not (<= lower v upper))
         (fault #:summary "a different number"
                #:expected (make-=-attribute e tolerance)
                #:actual (make-self-attribute v))))
  (expectation-rename (expect/singular make-fault) '=))
