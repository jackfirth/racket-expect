#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-log* (->* (any/c) (#:logger logger?) expectation?)]
  [the-log-context context?]))

(require expect/private/data
         expect/private/function
         expect/private/lite
         racket/function)


(struct log-context context () #:transparent)
(define the-log-context (log-context "the list of logged messages"))

(define (log-receiver-evts r)
  (define msg (sync/timeout (const #f) r))
  (if msg (cons msg (log-receiver-evts r)) '()))

(define (expect-log* msgs-exp* #:logger [logger (current-logger)])
  (define msgs-exp (expect/context (->expectation msgs-exp*) the-log-context))
  (define (around apply-exp)
    (define r (make-log-receiver logger 'debug))
    (define call-flts (apply-exp))
    (define msg-flts (expectation-apply msgs-exp (log-receiver-evts r)))
    (append call-flts msg-flts))
  (expect/around expect-not-raise around))
