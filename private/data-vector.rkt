#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-vector (rest-> expectation? expectation?)]
  [expect-vector-ref (-> expectation? exact-nonnegative-integer? expectation?)]
  [expect-vector-count (-> expectation? expectation?)]))

(require fancy-app
         "base.rkt"
         "combinator.rkt"
         "compare.rkt"
         "data-collect.rkt"
         "logic.rkt"
         "util.rkt")


(define (expect-vector-ref exp idx)
  (expect/context (expect/proc exp (vector-ref _ idx)) (index-context idx)))

(define (expect-vector-count exp)
  (expect/context (expect/proc exp vector-length) count-context))

(define (expect-vector . exps)
  (define (vec->items-exp vec)
    (apply expect-all
           (map/index expect-vector-ref (take/chop exps (vector->list vec)))))
  (expect-and (expect-pred vector?)
              (expect-all (expect-vector-count (expect-equal? (length exps)))
                          (expect/dependent vec->items-exp))))
