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
  (define anon-exp
    (expect/context (expect/proc exp (vector-ref _ idx)) (index-context idx)))
  (expectation-rename anon-exp 'vector-ref))

(define (expect-vector-count exp)
  (expectation-rename (expect/count exp vector-length) 'vector-count))

(define (expect-vector . exps)
  (define (vec->items-exp vec)
    (apply expect-all
           (map/index expect-vector-ref (take/chop exps (vector->list vec)))))
  (define exp
    (expect-and (expect-pred vector?)
                (expect-all (expect-vector-count (expect-equal? (length exps)))
                            (expect/dependent vec->items-exp))))
  (expectation-rename exp 'vector))
