#lang racket/base

(provide check-return)

(require (for-syntax racket/base
                     syntax/parse)
         arguments
         expect
         expect/rackunit)

(define-syntax (check-return stx)
  (syntax-parse stx
    [(_ (f:expr arg:expr ...) exp:expr)
     (syntax/loc stx
       (check-expect f (expect-call (arguments arg ...)
                                    (expect-return exp))))]))
