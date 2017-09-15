#lang racket/base

(require expect
         expect/rackunit
         racket/function
         (only-in rackunit test-case))


(test-case "expect-compare"
  (define small-exp (expect-compare < 10))
  (check-expect small-exp (expect-exp-faults 5))
  (define fault-exp
    (expect-fault #:expected (make-compare-attribute < 10)
                  #:actual (make-self-attribute 11)
                  #:contexts (list)))
  (check-expect small-exp (expect-exp-faults 11 fault-exp)))

(define b1 (box 'foo))
(define b2 (box 'foo))

(test-case "expect-eq?"
  (check-expect (expect-eq? 'foo) (expect-exp-faults 'foo))
  (check-expect (expect-eq? b1) (expect-exp-faults b1))
  (check-expect (expect-eq? b1) (expect-exp-faults b2 expect-any)))

(test-case "expect-eqv?"
  (check-expect (expect-eqv? (expt 10 20)) (expect-exp-faults (expt 10 20)))
  (check-expect (expect-eqv? b1) (expect-exp-faults b1))
  (check-expect (expect-eqv? b1) (expect-exp-faults b2 expect-any)))

(test-case "expect-equal?"
  (check-expect (expect-equal? b1) (expect-exp-faults b2)))

(test-case "expect-not-eq?"
  (check-expect (expect-not-eq? b1) (expect-exp-faults b2))
  (check-expect (expect-not-eq? b1) (expect-exp-faults b1 expect-any)))

(test-case "expect-not-eqv?"
  (check-expect (expect-not-eqv? (expt 10 20))
                (expect-exp-faults (expt 10 20) expect-any))
  (check-expect (expect-not-eqv? 'foo) (expect-exp-faults 'bar)))

(test-case "expect-not-equal?"
  (check-expect (expect-not-equal? (vector 1 2 3))
                (expect-exp-faults (vector 1 2 100)))
  (check-expect (expect-not-equal? (vector 1 2 3))
                (expect-exp-faults (vector 1 2 3) expect-any)))

(test-case "expect-="
  (define 10-exp (expect-= 10 0.1))
  (check-expect 10-exp (expect-exp-faults 10))
  (check-expect 10-exp (expect-exp-faults 10.001))
  (define fault-exp
    (expect-fault #:expected (make-=-attribute 10 0.1)
                  #:actual (make-self-attribute 10.5)))
  (check-expect 10-exp (expect-exp-faults 10.5 fault-exp)))

(test-case "attribute-sugar"
  (check-expect (make-eq-attribute 'foo) (make-compare-attribute eq? 'foo))
  (check-pred eqv-attribute? (make-compare-attribute eqv? 'foo)))
