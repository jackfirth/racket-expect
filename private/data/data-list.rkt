#lang racket/base

(require racket/contract)

(module+ for-conversion
  (provide
   (contract-out
    [expect-list (rest-> expectation? expectation?)]
    [expect-list-ref (-> expectation? exact-nonnegative-integer? expectation?)]
    [expect-list-count (-> expectation? expectation?)])))

(require fancy-app
         expect/private/base
         expect/private/combinator
         expect/private/logic
         expect/private/util
         "data-collect.rkt"
         (submod "compare.rkt" for-conversion))

(module+ test
  (require racket/function
           rackunit))


(define (expect-list-ref exp idx)
  (expect/context (expect/proc exp (list-ref _ idx)) (index-context idx)))

(define expect-list-count (expect/count _ length))

(define (expect-list . exps)
  (define (list->items-exp vs)
    (apply expect-all (map/index expect-list-ref (take/chop exps vs))))
  (expect-and (expect-pred list?)
              (expect-all (expect-list-count (expect-equal? (length exps)))
                          (expect/dependent list->items-exp))))
