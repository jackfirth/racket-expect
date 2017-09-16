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
  (test-case "box"
    (check-expect (expect-equal? b1) (expect-exp-faults b2)))
  (test-case "syntax"
    (define id-stx #'foo)
    (check-expect (expect-equal? id-stx) (expect-exp-faults id-stx))
    (test-case "wrong-contents"
      (define not-here-exp
        (expect-fault #:expected (make-equal-attribute 'here)
                      #:actual (make-self-attribute 'there)
                      #:contexts (list (expect-pred syntax-context?))))
      (check-expect (expect-equal? #'here)
                    (expect-exp-faults #'there not-here-exp)))
    (test-case "not-eq"
      (define other-id #'foo)
      (define not-eq-exp
        (expect-fault #:expected (make-eq-attribute id-stx)
                      #:actual (make-self-attribute other-id)
                      #:contexts (list)))
      (check-expect (expect-equal? id-stx)
                    (expect-exp-faults other-id not-eq-exp)))
    (define list-stx #'(a b c))
    (define list*-stx #'(a b . c))
    (test-case "list-contents"
      (define list-exp (expect-equal? list-stx))
      (check-expect list-exp (expect-exp-faults list-stx))
      (define fault-exp
        (expect-fault #:expected (make-not-attribute (make-self-attribute #f))
                      #:actual (make-self-attribute #f)
                      #:contexts (list (expect-pred syntax-context?))))
      (check-expect list-exp (expect-exp-faults list*-stx fault-exp)))
    (test-case "list*-contents"
      (define list*-exp (expect-equal? list*-stx))
      (check-expect list*-exp (expect-exp-faults list*-stx))
      (define fault-exp
        (expect-fault #:expected (make-equal-attribute (syntax-e list*-stx))
                      #:actual (make-self-attribute (syntax-e list-stx))
                      #:contexts (list (expect-pred syntax-context?))))
      (check-expect list*-exp (expect-exp-faults list-stx fault-exp)))))

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
