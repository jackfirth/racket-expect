#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-exp-faults
   (-> any/c (or/c (listof (or/c fault? expectation?)) expectation?)
       expectation?)]
  [expect-exp-no-faults (-> any/c expectation?)]
  [expect-exp-one-fault (->* (any/c) ((or/c fault? expectation?)) expectation?)]
  [expect-fault (->* ()
                     (#:summary (or/c string? expectation?)
                      #:expected (or/c attribute? expectation?)
                      #:actual (or/c attribute? expectation?)
                      #:contexts (or/c list? expectation?))
                     expectation?)]
  [expect-attribute (->* () ((or/c string? expectation?)) expectation?)]))

(require racket/list
         "base.rkt"
         "combinator.rkt"
         "data/main.rkt"
         "logic.rkt"
         "struct.rkt"
         "util-context.rkt")


(struct faults-context context (input) #:transparent)

(define (make-faults-context input)
  (faults-context (format "the faults found in input ~v" input) input))

(define (expect/proc+context exp proc ctxt)
  (expect/context (expect/proc exp proc) ctxt))

(define (expect-exp-faults input v)
  (define exp (->expectation v))
  (define (apply e) (expectation-apply e input))
  (expect-and (expect-pred expectation?)
              (expect/proc+context exp apply (make-faults-context input))))

(define (expect-exp-no-faults input)
  (expect-exp-faults input (expect-pred empty?)))

(define (expect-exp-one-fault input [v expect-any])
  (expect-exp-faults input (expect-list v)))

(define-singleton-contexts
  fault-summary-context "the summary field of the fault"
  fault-expected-context "the expected field of the fault"
  fault-actual-context "the actual field of the fault"
  fault-contexts-context "the contexts field of the fault"
  attribute-description-context "the description field of the attribute")

(define-struct-expectation fault)

(define (expect-attribute [attr expect-any])
  (expect-struct attribute [attribute-description attr]))
