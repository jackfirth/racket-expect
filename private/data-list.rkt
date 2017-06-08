#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-list (rest-> expectation? expectation?)]
  [expect-list-ref (-> expectation? exact-nonnegative-integer? expectation?)]
  [expect-list-count (-> expectation? expectation?)]))

(require fancy-app
         "base.rkt"
         "combinator.rkt"
         "compare.rkt"
         "data-collect.rkt"
         "logic.rkt"
         "util.rkt")

(module+ test
  (require rackunit))


(define (expect-list-ref exp idx)
  (expect/context (expect/proc exp (list-ref _ idx)) (index-context idx)))

(define (expect-list-count exp)
  (expect/context (expect/proc exp length) count-context))

(define (expect-list . exps)
  (define (list->items-exp vs)
    (apply expect-all (map/index expect-list-ref (take/chop exps vs))))
  (expect-and (expect-pred list?)
              (expect-all (expect-list-count (expect-equal? (length exps)))
                          (expect/derive list->items-exp))))
