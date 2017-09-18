#lang racket/base

(module+ for-conversion
  (require racket/contract/base)

  (provide
   (contract-out
    [expect-syntax (-> expectation? expectation?)]
    [expect-syntax-list (-> expectation? expectation?)])))

(require expect/private/lite
         (submod "context.rkt" for-internal))


(define (expect-syntax exp)
  (define anon-exp
    (expect-and (expect-pred syntax?)
                (expect/context (expect/proc exp syntax-e)
                                the-syntax-e-context)))
  (expectation-rename anon-exp 'syntax))

(define (expect-syntax-list exp)
  (expect-and (expect-pred syntax?)
              (expect/context (expect/proc (expect-and expect-not-false exp)
                                           syntax->list)
                              the-syntax-list-context)))
