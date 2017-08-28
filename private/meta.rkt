#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-exp-faults (-> any/c expectation-convertible? expectation?)]
  [expect-exp-no-faults (-> any/c expectation?)]
  [expect-exp-one-fault (->* (any/c) (expectation-convertible?) expectation?)]
  [expect-fault (->* ()
                     (#:summary expectation-convertible?
                      #:expected expectation-convertible?
                      #:actual expectation-convertible?
                      #:contexts expectation-convertible?)
                     expectation?)]
  [expect-attribute (->* () (expectation-convertible?) expectation?)]))

(require racket/list
         racket/function
         syntax/parse/define
         "base.rkt"
         "combinator.rkt"
         "convert-base.rkt"
         "logic.rkt"
         "util-context.rkt"
         (submod "data-list.rkt" no-conversion))


(struct faults-context context (input) #:transparent)

(define (make-faults-context input)
  (faults-context (format "the faults found in input ~v" input) input))

(define (expect/proc+context exp proc ctxt)
  (expect/context (expect/proc exp proc) ctxt))

(define (expect-exp-faults input exp)
  (define (apply e) (expectation-apply e input))
  (define exp* (expectation-convert exp))
  (expect-and (expect-pred expectation?)
              (expect/proc+context exp apply (make-faults-context input))))

(define expect-any (expectation (const '())))

(define (expect-exp-no-faults input)
  (expect-exp-faults input (expect-pred empty?)))

(define (expect-exp-one-fault input [exp expect-any])
  (expect-exp-faults input (expect-list exp)))

(define-singleton-contexts
  fault-summary-context "the summary field of the fault"
  fault-expected-context "the expected field of the fault"
  fault-actual-context "the actual field of the fault"
  fault-contexts-context "the contexts field of the fault"
  attribute-description-context "the description field of the attribute")

(define (expect-fault #:summary [sum expect-any]
                      #:actual [act expect-any]
                      #:expected [exp expect-any]
                      #:contexts [ctxs expect-any])
  (define sum* (expectation-convert sum))
  (define exp* (expectation-convert exp))
  (define act* (expectation-convert act))
  (define ctxs* (expectation-convert ctxs))
  (define fields-exp
    (expect-all
     (expect/proc+context sum* fault-summary fault-summary-context)
     (expect/proc+context act* fault-actual fault-actual-context)
     (expect/proc+context exp* fault-expected fault-expected-context)
     (expect/proc+context ctxs* fault-contexts fault-contexts-context)))
  (expect-and (expect-pred fault?) fields-exp))

(define (expect-attribute [attr expect-any])
  (expect-and (expect-pred attribute?)
              (expect/proc+context (expectation-convert attr)
                                   attribute-description
                                   attribute-description-context)))
