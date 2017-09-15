#lang racket/base

(require arguments
         expect
         expect/rackunit
         (only-in rackunit test-case)
         "util.rkt")

(struct test-attribute attribute () #:transparent)
(define test-attr (test-attribute "test attr"))

(struct test-context context () #:transparent)
(define test-ctxt (test-context "test ctxt"))

(define test-fault
  (fault #:summary "some fault"
         #:actual (make-self-attribute 'foo)
         #:expected test-attr
         #:contexts (list test-ctxt)))

(test-case "expect-fault"
  (check-expect test-fault (expect-fault))
  (check-expect test-fault (expect-fault #:summary "some fault"))
  (check-expect test-fault (expect-fault #:actual (expect-pred self-attribute?)))
  (check-expect test-fault (expect-fault #:expected test-attr))
  (check-expect test-fault (expect-fault #:contexts (list test-ctxt)))
  (check-expect test-fault (expect-fault #:contexts (expect-pred list?)))
  (check-expect (expect-fault #:summary "some other fault")
                (expect-exp-faults test-fault expect-any)))
