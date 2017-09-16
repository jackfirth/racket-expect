#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [the-length-context splice-context?]))

(module+ for-conversion
  (provide
   (contract-out
    [expect-list (rest-> expectation? expectation?)]
    [expect-list-ref (-> expectation? exact-nonnegative-integer? expectation?)]
    [expect-list-count (-> expectation? expectation?)])))

(require arguments
         fancy-app
         expect/private/lite
         expect/private/util
         "context.rkt"
         "kernel-apply.rkt")

(module+ test
  (require racket/function
           rackunit))


(define the-length-context (make-apply1-context length))

(define (expect-list-ref exp idx)
  (define anon-exp
    (expect/context (expect/proc exp (list-ref _ idx))
                    (make-sequence-context idx)))
  (expectation-rename anon-exp 'list-ref))

(define (expect-list-count e)
  (expectation-rename (expect-apply1 length e) 'list-count))

(define (expect-list-items . exps)
  (define (list->items-exp vs)
    (apply expect-all (map/index expect-list-ref (take/chop exps vs))))
  (expect/dependent list->items-exp))

(define (expect-list . exps)
  (define exp
    (expect-and (expect-pred list?)
                (expect-all (expect-list-count (expect-eqv? (length exps)))
                            (apply expect-list-items exps))))
  (expectation-rename exp 'list))
