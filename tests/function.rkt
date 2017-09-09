#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         arguments
         expect
         expect/rackunit
         racket/function)

(define (expect-attribute descr)
  (expect-struct attribute [attribute-description descr]))

(define (expect-expects input descr)
  (expect-exp-one-fault input
                        (expect-fault #:expected (expect-attribute descr))))

(define (raise-foo) (raise 'foo))

(define-syntax (check-return stx)
  (syntax-parse stx
    [(_ (f:expr arg:expr ...) exp:expr)
     (syntax/loc stx
       (check-expect f (expect-call (arguments arg ...)
                                    (expect-return exp))))]))

(check-expect void expect-not-raise)
(check-expect expect-not-raise (expect-expects raise-foo "no value raised"))
(check-expect raise-foo (expect-raise 'foo))
(check-return (expect-raise 'foo) (expect-expects void "raised a value"))
(check-return (expect-raise 'bar) (expect-expects raise-foo "equal? to 'bar"))
(check-expect (thunk 'foo) (expect-return 'foo))
(check-expect values (expect-return))
(check-expect (thunk (values 'foo 'bar)) (expect-return 'foo 'bar))
(check-return (expect-return 'foo)
              (expect-expects (thunk 'bar) "equal? to 'foo"))
