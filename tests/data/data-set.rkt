#lang racket/base

(require expect
         expect/rackunit
         fancy-app
         racket/function
         racket/set
         (only-in rackunit test-case))


(define expect-exp-set-pred-fault
  (expect-exp-faults 'not-a-set
                     (expect-fault #:expected (make-pred-attribute set?)
                                   #:actual (make-self-attribute 'not-a-set)
                                   #:contexts (list))))

(test-subject "expect-set-member?" #:subject (expect-set-member? 'foo)
  (expect-exp-faults (set 'foo))
  (expect-exp-faults (set 'foo 'bar))
  (expect-exp-faults (set 'bar)
                     (expect-fault
                      #:expected (make-contains-attribute set-member? 'foo)
                      #:actual (make-self-attribute (set 'bar))
                      #:contexts (list)))
  expect-exp-set-pred-fault)

(test-subject "expect-set-not-member?" #:subject (expect-set-not-member? 'foo)
  (expect-exp-faults (set 'bar))
  (expect-exp-faults (set))
  (expect-exp-faults (set 'bar 'baz 'blah))
  (expect-exp-faults (set 'bar 'foo)
                     (expect-fault
                      #:expected (make-not-attribute
                                  (make-contains-attribute set-member? 'foo))
                      #:actual (make-self-attribute (set 'bar 'foo))
                      #:contexts (list)))
  expect-exp-set-pred-fault)

(test-case "expect-set-count"
  (test-subject #:subject (expect-set-count (expect-pred even?))
    (expect-exp-faults (set 1 2))
    (expect-exp-faults (set))
    (expect-exp-faults (set 1 2 3)
                       (expect-fault
                        #:expected (make-pred-attribute even?)
                        #:actual (make-self-attribute 3)
                        #:contexts (list the-length-context)))
    expect-exp-set-pred-fault)
  (test-subject "conversion" #:subject (expect-set-count 3)
    (expect-exp-faults (set 1 2 3))
    (expect-exp-faults (set 1)
                       (expect-fault #:expected (make-equal-attribute 3)
                                     #:actual (make-self-attribute 1)))))

(define (expect-subset-fault actual-st extra-st)
  (define attr (make-contains-none-attribute set-member? (set->list extra-st)))
  (expect-fault #:expected attr
                #:actual (make-self-attribute actual-st)
                #:contexts (list)))

(define (expect-superset-fault actual-st missing-st)
  (define attr (make-contains-all-attribute set-member? (set->list missing-st)))
  (expect-fault #:expected attr
                #:actual (make-self-attribute actual-st)
                #:contexts (list)))

(test-subject "expect-subset" #:subject (expect-subset (set 1 2 3))
  (expect-exp-faults (set 1))
  (expect-exp-faults (set 1 2 3))
  (expect-exp-faults (set 1 2 3 4 5)
                     (expect-subset-fault (set 1 2 3 4 5) (set 4 5)))
  (expect-exp-faults (set 1 4 5) (expect-subset-fault (set 1 4 5) (set 4 5)))
  expect-exp-set-pred-fault)

(test-subject "expect-superset" #:subject (expect-superset (set 1 2 3))
  (expect-exp-faults (set 1 2 3 4 5))
  (expect-exp-faults (set 1 2 3))
  (expect-exp-faults (set 1) (expect-superset-fault (set 1) (set 2 3)))
  (expect-exp-faults (set 1 4 5) (expect-superset-fault (set 1 4 5) (set 2 3)))
  expect-exp-set-pred-fault)

(test-subject "expect-set" #:subject (expect-set 1 2)
  (expect-exp-faults (set 1 2))
  (expect-exp-faults (set 1) (expect-superset-fault (set 1) (set 2)))
  (expect-exp-faults (set 1 2 3) (expect-subset-fault (set 1 2 3) (set 3)))
  (expect-exp-faults (set 1 3)
                     (expect-superset-fault (set 1 3) (set 2))
                     (expect-subset-fault (set 1 3) (set 3)))
  expect-exp-set-pred-fault)
