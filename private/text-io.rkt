#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-output (->* ((or/c string? regexp? expectation?))
                      (#:call expectation?)
                      expectation?)]
  [the-output-context context?]))

(require expect/private/data
         expect/private/function
         expect/private/lite
         expect/private/text)


(struct output-context context () #:transparent)
(define the-output-context
  (output-context "the string written to the output port"))

(define (expect-output exp* #:call [call-exp expect-not-raise])
  (define exp (if (regexp? exp*) (expect-regexp-match exp*) (->expectation exp*)))
  (define (around apply-exp)
    (define buf (open-output-string))
    (define call-flts (parameterize ([current-output-port buf]) (apply-exp)))
    (define out-flts
      (expectation-apply (expect/context exp the-output-context)
                         (get-output-string buf)))
    (append call-flts out-flts))
  (expect/around call-exp around))
