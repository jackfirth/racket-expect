#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-true expectation?]
  [expect-false expectation?]
  [expect-not-false expectation?]
  [expect-pred (-> predicate/c expectation?)]
  [expect-all (rest-> expectation? expectation?)]
  [expect-and (rest-> expectation? expectation?)]
  [expect-conjoin (rest-> predicate/c expectation?)]
  [expect-disjoin (rest-> predicate/c expectation?)]
  [struct (not-attribute attribute)
    ([description string?] [negated attribute?]) #:omit-constructor]
  [make-not-attribute (-> attribute? not-attribute?)]
  [struct (or-attribute attribute)
    ([description string?] [cases (listof attribute?)]) #:omit-constructor]
  [make-or-attribute (->* ((listof attribute?))
                          (#:description (or/c string? #f))
                          or-attribute?)]
  [struct (and-attribute attribute)
    ([description string?] [cases (listof attribute?)]) #:omit-constructor]
  [make-and-attribute (->* ((listof attribute?))
                           (#:description (or/c string? #f))
                           and-attribute?)]
  [struct (pred-attribute attribute)
    ([description string?] [value predicate/c]) #:omit-constructor]
  [make-pred-attribute (-> predicate/c pred-attribute?)]))

(require fancy-app
         expect/private/util
         racket/function
         racket/list
         racket/stream
         racket/string
         "base.rkt"
         "combinator.rkt")

(module+ test
  (require rackunit))

;; Booleans

(define (true-fault v)
  (and (not (equal? v #t))
       (fault #:summary "true"
              #:expected (make-self-attribute #t)
              #:actual (make-self-attribute v))))

(define expect-true (expectation-rename (expect/singular true-fault) 'true))

(define (false-fault v)
  (and v
       (fault #:summary "false"
              #:expected (make-self-attribute #f)
              #:actual (make-self-attribute v))))

(define expect-false (expectation-rename (expect/singular false-fault) 'false))

(struct not-attribute attribute (negated) #:transparent)
(define (make-not-attribute negated)
  (not-attribute (format "not ~a" (attribute-description negated)) negated))

(define (not-false-fault v)
  (and (not v)
       (fault #:summary "not false"
              #:expected (make-not-attribute (make-self-attribute #f))
              #:actual (make-self-attribute v))))

(define expect-not-false
  (expectation-rename (expect/singular not-false-fault) 'not-false))

;; Logical / predicate combinators

(struct pred-attribute attribute (value) #:transparent)
(define (make-pred-attribute pred)
  (pred-attribute (format "value satisfying ~a" (object-name pred)) pred))

(define (pred-fault pred v)
  (and (not (pred v))
       (fault #:summary "a different kind of value"
              #:expected (make-pred-attribute pred)
              #:actual (make-self-attribute v))))

(define (expect-pred pred)
  (expectation-rename (expect/singular (pred-fault pred _))
                      (object-name pred)))

(define (expect-all . exps)
  (define (apply-all v) (append-map (expectation-apply _ v) exps))
  (expectation apply-all #:name 'all))

(define (expect-and . exps)
  (define (apply-and v)
    (define faults-stream (stream-map (expectation-apply _ v) exps))
    (or (for/first ([faults (in-stream faults-stream)]
                    #:unless (empty? faults))
          faults)
        (list)))
  (expectation apply-and #:name 'and))

(struct or-attribute attribute (cases) #:transparent)
(struct and-attribute attribute (cases) #:transparent)

(define ((make-join-attribute construct before-last)
         cases #:description [desc* #f])
  (define desc
    (or desc* (string-join (map attribute-description cases) ", "
                           #:before-last before-last)))
  (construct desc cases))

(define make-or-attribute (make-join-attribute or-attribute " or "))
(define make-and-attribute (make-join-attribute and-attribute " and "))

(define (expect-conjoin . preds)
  (expectation-rename (apply expect-and (map expect-pred preds)) 'conjoin))

(define (expect-disjoin . preds)
  (define (disjoin-fault v)
    (define (app pred) (pred v))
    (and (not (ormap app preds))
         (fault #:summary "a different kind of value"
                #:expected (make-or-attribute (map make-pred-attribute preds))
                #:actual (make-self-attribute v))))
  (expectation-rename (expect/singular disjoin-fault) 'disjoin))
