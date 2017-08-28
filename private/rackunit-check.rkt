#lang racket/base

(require (only-in rackunit define-check)
         arguments
         expect
         syntax/parse/define
         (submod "rackunit.rkt" for-custom-checks))

(define-simple-macro
  (define-expect-checks [(id:id arg:id ...+) subject:expr expectation:expr] ...+)
  (begin
    (begin
      (define-check (id arg ...) (check-expect* subject expectation))
      (provide id))
    ...))

(define-expect-checks
  [(check-eq? a b) a (expect-eq? b)]
  [(check-eqv? a b) a (expect-eqv? b)]
  [(check-equal? a b) a (expect-equal? b)]
  [(check-not-eq? a b) a (expect-not-eq? b)]
  [(check-not-eqv? a b) a (expect-not-eqv? b)]
  [(check-not-equal? a b) a (expect-not-equal? b)]
  [(check-pred p v) v (expect-pred p)]
  [(check-= a b eps) a (expect-= b eps)]
  [(check-true v) v expect-true]
  [(check-false v) v expect-false]
  [(check-not-false v) v expect-not-false]
  [(check-exn p f) f (expect-raise p)]
  [(check-not-exn p f) f expect-not-raise]
  [(check op a b) op (expect-call (arguments a b) (expect-return #t))])
