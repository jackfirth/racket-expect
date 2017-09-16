#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [rename expect-equal? expect-equal? (-> any/c exp?)]
  [rename cvrt:expect-hash expect-hash (rest->* (list any/c any/c) exp?)]))

(require racket/list
         racket/math
         racket/set
         expect/private/base
         expect/private/combinator
         expect/private/compare
         expect/private/logic
         expect/private/util
         "data-set.rkt"
         "convert-base.rkt"
         (submod "convert-base.rkt" for-conversion)
         (submod "data-hash.rkt" for-conversion)
         (submod "data-list.rkt" for-conversion)
         (submod "data-set.rkt" for-conversion)
         (submod "data-vector.rkt" for-conversion)
         (submod "data-syntax.rkt" for-conversion))


(define exp? expectation?)

(define (expect-equal? v)
  (cond
    [(list? v) (apply expect-list (map expect-equal? v))]
    [(vector? v)
     (apply expect-vector (map expect-equal? (vector->list v)))]
    [(set? v) (apply expect-set (set->list v))]
    [(hash? v)
     (define converted (map expect-equal? (hash-values v)))
     (apply expect-hash (append-map list (hash-keys v) converted))]
    [(syntax? v)
     (if (syntax->list v)
         (expect-and (expect-syntax-list (expect-equal? (syntax->list v)))
                     (expect-eq? v))
         (expect-and (expect-syntax (expect-equal? (syntax-e v)))
                     (expect-eq? v)))]
    [else (expect-compare equal? v)]))

(define (cvrt:expect-hash . k+vs)
  (define converted (map ->expectation (slice k+vs #:start 1 #:step 2)))
  (apply expect-hash (append-map list (slice k+vs #:step 2) converted)))

(define-conversions
  [(expect-hash-count <convert>) (-> (or/c natural? exp?) exp?)]
  [(expect-hash-ref k <convert>) (-> any/c any/c exp?)]
  [(expect-hash-keys <convert>) (-> (or/c set? exp?) exp?)]
  [(expect-list . <convert>) (rest-> any/c exp?)]
  [(expect-list-ref <convert> v) (-> any/c natural? exp?)]
  [(expect-list-count <convert>) (-> (or/c natural? exp?) exp?)]
  [(expect-set-count <convert>) (-> (or/c natural? exp?) exp?)]

  ;; expect-syntax with conversion makes expect-syntax-list unnecessary due to
  ;; how conversion converts syntax list objects into expectations that flatten
  ;; their input first
  [(expect-syntax <convert>) (-> any/c exp?)]

  [(expect-vector . <convert>) (rest-> any/c exp?)]
  [(expect-vector-ref <convert> v) (-> any/c natural? exp?)]
  [(expect-vector-count <convert>) (-> (or/c natural? exp?) exp?)])
