#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
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
                      #:contexts (or/c list? expectation?))
                     expectation?)]
  [expect-attribute (->* () ((or/c string? expectation?)) expectation?)]))

(require (except-in "base.rkt"
                    fault
                    fault?
                    fault-summary
                    fault-expected
                    fault-actual
                    fault-contexts)
         (submod "base.rkt" for-meta)
         "combinator.rkt"
         "data/main.rkt"
         "logic.rkt"
         "struct.rkt")


(struct faults-context context (input) #:transparent)

(define (make-faults-context input)
  (faults-context (format "the faults found in input ~v" input) input))

(define (expect-exp-faults input . vs)
  (expectation-rename (expect-exp-faults* input vs) 'faults))

(define (expect-exp-faults* input v)
  (define exp (->expectation v))
  (define (apply e) (expectation-apply e input))
  (define anon-exp
    (expect-and (expect-pred expectation?)
                (expect/context (expect/proc exp apply)
                                (make-faults-context input))))
  (expectation-rename anon-exp 'faults*))

(define-struct-expectation fault)

(define (expect-attribute [attr expect-any])
  (expect-struct attribute [attribute-description attr]))
