#lang racket/base

(require arguments
         expect
         expect/rackunit
         (only-in rackunit test-case)
         "util.rkt")

(struct test-attribute attribute () #:transparent)
(define test-attr (test-attribute "test attr"))

(test-case "expect-attribute"
  (check-expect test-attr (expect-attribute))
  (check-expect test-attr (expect-attribute "test attr"))
  (check-expect (expect-attribute "blah")
                (expect-exp-faults test-attr expect-any)))

(define test-fault
  (fault #:summary "some fault"
         #:actual (make-self-attribute 'foo)
         #:expected test-attr))

(test-case "expect-fault"
  (check-expect test-fault (expect-fault))
  (check-expect test-fault (expect-fault #:summary "some fault"))
  (check-expect test-fault (expect-fault #:actual (expect-pred self-attribute?)))
  (check-expect test-fault (expect-fault #:expected test-attr))
  (check-expect (expect-fault #:summary "some other fault")
                (expect-exp-faults test-fault expect-any)))
