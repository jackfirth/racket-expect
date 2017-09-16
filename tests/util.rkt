#lang racket/base

(provide check-fail)

(require arguments
         expect
         expect/rackunit
         rackunit/log
         (only-in rackunit
                  exn:test:check?
                  current-check-handler
                  define-check))


(define ((call-check chk))
  (parameterize ([current-check-handler raise] [test-log-enabled? #f]) (chk)))

(define (expect-check-fail . args)
  (expect-call (apply arguments args)
               (expect/proc (expect-raise (expect-pred exn:test:check?))
                            call-check)))

(define-check (check-fail f args)
  (fail-check/expect f (apply/arguments expect-check-fail args)))
