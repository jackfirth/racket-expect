#lang racket/base

(require racket/contract)

(provide (contract-out
          [rename expect-equal?/convert expect-equal?
                  (-> any/c expectation?)]
          [rename expect-hash/convert expect-hash
                  (rest->* (list any/c cvrt?) exp?)]))

(require (for-syntax racket/base
                     racket/syntax)
         racket/list
         racket/set
         "base.rkt"
         "convert-base.rkt"
         "data-set.rkt"
         "util.rkt"
         (submod "compare.rkt" no-conversion)
         (submod "data-hash.rkt" no-conversion)
         (submod "data-list.rkt" no-conversion)
         (submod "data-set.rkt" no-conversion)
         (submod "data-vector.rkt" no-conversion)
         (submod "function.rkt" no-conversion))

(module+ test
  (require rackunit))


(define exp? expectation?)
(define cvrt? expectation-convertible?)

(define (expect-equal?/convert v)
  (cond
    [(list? v) (apply expect-list (map expect-equal?/convert v))]
    [(vector? v)
     (apply expect-vector (map expect-equal?/convert (vector->list v)))]
    [(set? v) (apply expect-set (set->list v))]
    [(hash? v)
     (define converted (map expect-equal?/convert (hash-values v)))
     (apply expect-hash (append-map list (hash-keys v) converted))]
    [else (expect-equal? v)]))

(define (expect-hash/convert . k+vs)
  (define converted (map expectation-convert (slice k+vs #:start 1 #:step 2)))
  (apply expect-hash (append-map list (slice k+vs #:step 2) converted)))

(define-conversions
  [(expect-hash-count <convert>) (-> cvrt? exp?)]
  [(expect-hash-ref k <convert>) (-> any/c cvrt? exp?)]
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
