#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [syntax-context? predicate/c]))

(module+ for-conversion
  (provide
   (contract-out
    [expect-syntax (-> expectation? expectation?)]
    [expect-syntax-list (-> expectation? expectation?)])))

(require expect/private/base
         expect/private/logic
         "kernel-apply.rkt")


(define (syntax-context? v)
  (or (equal? v (make-apply1-context syntax-e))
      (equal? v (make-apply1-context syntax->list))))      

(define (expect-syntax exp)
  (define anon-exp
    (expect-and (expect-pred syntax?) (expect-apply1 syntax-e exp)))
  (expectation-rename anon-exp 'syntax))

(define (expect-syntax-list exp)
  (expect-and (expect-pred syntax?)
              (expect-apply1 syntax->list (expect-and expect-not-false exp))))
