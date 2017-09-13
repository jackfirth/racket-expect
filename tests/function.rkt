#lang racket/base

(require expect
         expect/rackunit
         racket/function
         "function-util.rkt")

(define (expect-attribute descr)
  (expect-struct attribute [attribute-description descr]))

(define (expect-expects input descr)
  (expect-exp-one-fault input
                        (expect-fault #:expected (expect-attribute descr))))

(define (raise-foo) (raise 'foo))

(check-expect void expect-not-raise)
(check-expect expect-not-raise (expect-expects raise-foo "nothing"))
(check-expect raise-foo (expect-raise 'foo))
(check-return (expect-raise 'foo) (expect-expects void "anything"))
(check-return (expect-raise 'bar) (expect-expects raise-foo "equal? to 'bar"))
(check-expect (thunk 'foo) (expect-return 'foo))
(check-expect values (expect-return))
(check-expect (thunk (values 'foo 'bar)) (expect-return 'foo 'bar))
(check-return (expect-return 'foo)
              (expect-expects (thunk 'bar) "equal? to 'foo"))
(check-expect (expect-return 'foo)
              (expect-expects identity "arity accepting 0 arguments"))
(check-expect (expect-return 'foo) (expect-expects raise-foo "nothing"))
