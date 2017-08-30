#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [rename cvrt:expect-equal? expect-equal? (-> any/c exp?)]
  [rename cvrt:expect-hash expect-hash (rest->* (list any/c any/c) exp?)]))

(require (for-syntax racket/base
                     racket/syntax)
         racket/list
         racket/math
         racket/set
         expect/private/base
         expect/private/util
         "data-set.rkt"
         "convert-base.rkt"
         (submod "convert-base.rkt" for-conversion)
         (submod "compare.rkt" for-conversion)
         (submod "data-hash.rkt" for-conversion)
         (submod "data-list.rkt" for-conversion)
         (submod "data-set.rkt" for-conversion)
         (submod "data-vector.rkt" for-conversion))


(define exp? expectation?)

(define (cvrt:expect-equal? v)
  (cond
    [(list? v) (apply expect-list (map cvrt:expect-equal? v))]
    [(vector? v)
     (apply expect-vector (map cvrt:expect-equal? (vector->list v)))]
    [(set? v) (apply expect-set (set->list v))]
    [(hash? v)
     (define converted (map cvrt:expect-equal? (hash-values v)))
     (apply expect-hash (append-map list (hash-keys v) converted))]
    [else (expect-equal? v)]))

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
  [(expect-vector . <convert>) (rest-> any/c exp?)]
  [(expect-vector-ref <convert> v) (-> any/c natural? exp?)]
  [(expect-vector-count <convert>) (-> (or/c natural? exp?) exp?)])
