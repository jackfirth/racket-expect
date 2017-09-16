#lang racket/base

(require expect
         expect/rackunit
         (only-in rackunit test-case))


(test-subject "expect-compare" #:subject (expect-compare < 10)
  (expect-exp-faults 5)
  (expect-exp-faults 11
                     (expect-fault #:expected (make-compare-attribute < 10)
                                   #:actual (make-self-attribute 11)
                                   #:contexts (list))))

(define b1 (box 'foo))
(define b2 (box 'foo))

(test-subject "expect-eq?" #:subject (expect-eq? b1)
  (expect-exp-faults b1)
  (expect-exp-faults b2 expect-any))

(test-case "expect-eqv?"
  (check-expect (expect-eqv? (expt 10 20)) (expect-exp-faults (expt 10 20)))
  (test-subject #:subject (expect-eqv? b1)
    (expect-exp-faults b1)
    (expect-exp-faults b2 expect-any)))

(test-subject "expect-not-eq?" #:subject (expect-not-eq? b1)
  (expect-exp-faults b2)
  (expect-exp-faults b1 expect-any))

(test-case "expect-not-eqv?"
  (check-expect (expect-not-eqv? (expt 10 20))
                (expect-exp-faults (expt 10 20) expect-any))
  (check-expect (expect-not-eqv? 'foo) (expect-exp-faults 'bar)))

(test-subject "expect-not-equal?" #:subject (expect-not-equal? (vector 1 2 3))
  (expect-exp-faults (vector 1 2 100))
  (expect-exp-faults (vector 1 2 3) expect-any))

(test-subject "expect-=" #:subject (expect-= 10 0.1)
  (expect-exp-faults 10)
  (expect-exp-faults 10.001)
  (expect-exp-faults 10.5
                     (expect-fault #:expected (make-=-attribute 10 0.1)
                                   #:actual (make-self-attribute 10.5))))

(test-case "attribute-sugar"
  (check-expect (make-eq-attribute 'foo) (make-compare-attribute eq? 'foo))
  (check-pred eqv-attribute? (make-compare-attribute eqv? 'foo)))
