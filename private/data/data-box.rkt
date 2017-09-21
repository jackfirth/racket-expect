#lang racket/base

(module+ for-conversion
  (require racket/contract/base)
  (provide
   (contract-out
    [expect-box (-> expectation? expectation?)])))

(require expect/private/lite
         "context.rkt")


(define (expect-box exp)
  (expect-and (expect-pred box?)
              (expect/context (expect/proc exp unbox) the-box-context)))
