#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-exp-apply (-> any/c expectation? expectation?)]
  [expect-exp-faults (->* (any/c)
                          #:rest (listof (or/c fault? expectation?))
                          expectation?)]
  [expect-exp-faults* (-> any/c
                          (or/c expectation?
                                (listof (or/c fault? expectation?)))
                          expectation?)]
  [expect-fault (->* ()
                     (#:summary (or/c string? expectation?)
                      #:expected (or/c attribute? expectation?)
                      #:actual (or/c attribute? expectation?)
                      #:contexts (or/c expectation?
                                       (listof (or/c context? expectation?))))
                     expectation?)]
  [struct (expect-context context)
    ([description string?] [input any/c]) #:omit-constructor]
  [make-expect-context (-> any/c expect-context?)]))

(require (except-in "lite.rkt"
                    fault
                    fault?
                    fault-summary
                    fault-expected
                    fault-actual
                    fault-contexts)
         "data.rkt"
         "function.rkt"
         "struct.rkt"
         (submod "lite/base.rkt" for-meta))


(struct expect-context context (input) #:transparent)
(define (make-expect-context input)
  (expect-context (format "the expectation applied to ~v" input) input))

(define (expect-exp-apply input apply-exp)
  (define ((exp->thunk e)) (expectation-apply e input))
  (define exp
    (expect-and (expect-pred expectation?)
                (expect/proc (expect/context apply-exp
                                             (make-expect-context input))
                             exp->thunk)))
  (expectation-rename exp 'exp-apply))

(define (exp-app/return input exp name)
  (expectation-rename (expect-exp-apply input (expect-return exp)) name))
  
(define (expect-exp-faults input . vs) (exp-app/return input vs 'faults))
(define (expect-exp-faults* input exp) (exp-app/return input exp 'faults*))

(define-struct-expectation fault)
