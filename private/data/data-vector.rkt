#lang racket/base

(require racket/contract)

(module+ for-conversion
  (provide
   (contract-out
    [expect-vector (rest-> expectation? expectation?)]
    [expect-vector-ref (-> expectation? exact-nonnegative-integer? expectation?)]
    [expect-vector-count (-> expectation? expectation?)])))

(require fancy-app
         expect/private/base
         expect/private/combinator
         expect/private/logic
         expect/private/util
         "data-collect.rkt"
         (submod "compare.rkt" for-conversion))


(define (expect-vector-ref exp idx)
  (expect/context (expect/proc exp (vector-ref _ idx)) (index-context idx)))

(define expect-vector-count (expect/count _ vector-length))

(define (expect-vector . exps)
  (define (vec->items-exp vec)
    (apply expect-all
           (map/index expect-vector-ref (take/chop exps (vector->list vec)))))
  (expect-and (expect-pred vector?)
              (expect-all (expect-vector-count (expect-equal? (length exps)))
                          (expect/dependent vec->items-exp))))
