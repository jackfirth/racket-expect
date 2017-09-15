#lang racket/base

(require racket/contract)

(module+ for-conversion
  (provide
   (contract-out
    [expect-list (rest-> expectation? expectation?)]
    [expect-list-ref (-> expectation? exact-nonnegative-integer? expectation?)]
    [expect-list-count (-> expectation? expectation?)])))

(module+ for-count
  (provide
   (contract-out
    [expect/count
     (-> expectation? (-> any/c exact-nonnegative-integer?) expectation?)])))

(require arguments
         fancy-app
         expect/private/base
         expect/private/combinator
         expect/private/compare
         expect/private/function-kernel
         expect/private/logic
         expect/private/util
         "context.rkt"
         (submod expect/private/function-kernel no-reprovide))

(module+ test
  (require racket/function
           rackunit))


(define (expect/count exp count-proc)
  (expect/proc (expect-apply count-proc
                             (expect-return*/kernel (expect-list/kernel exp)))
               arguments))

(define (expect-list-ref exp idx)
  (define anon-exp
    (expect/context (expect/proc exp (list-ref _ idx))
                    (make-sequence-context idx)))
  (expectation-rename anon-exp 'list-ref))

(define (expect-list-count e)
  (expectation-rename (expect/count e length) 'list-count))

(define (expect-list/kernel . exps)
  (define (list->items-exp vs)
    (apply expect-all (map/index expect-list-ref (take/chop exps vs))))
  (expect/dependent list->items-exp))

(define (expect-list . exps)
  (define exp
    (expect-and (expect-pred list?)
                (expect-all (expect-list-count (expect-eqv? (length exps)))
                            (apply expect-list/kernel exps))))
  (expectation-rename exp 'list))
