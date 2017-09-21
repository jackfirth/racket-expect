#lang racket/base

(require expect
         expect/rackunit
         (only-in rackunit test-case))

(test-case "expect-list"
  (test-subject #:subject (expect-list (expect-equal? 1) expect-any)
    (expect-exp-faults '(1 2))
    (expect-exp-faults '(1 100))
    (expect-exp-faults '(100 2)
                       (expect-fault #:expected (make-equal-attribute 1)
                                     #:actual (make-self-attribute 100)
                                     #:contexts (list
                                                 (make-sequence-context 0))))
    (expect-exp-faults '(1 2 3)
                       (expect-fault #:expected (make-eqv-attribute 2)
                                     #:actual (make-self-attribute 3)
                                     #:contexts (list the-length-context))))
  (test-subject "conversion" #:subject (expect-list '(1 2) #(3 4))
    (expect-exp-faults '((1 2) #(3 4)))
    (expect-exp-faults '((1 100) #(3 4))
                       (expect-fault
                        #:contexts (list (make-sequence-context 0)
                                         (make-sequence-context 1))))))

(test-subject "expect-vector" #:subject (expect-vector 1 2)
  (expect-exp-faults #(1 2))
  (expect-exp-faults #(1 10) expect-any))

(test-subject "expect-list-ref" #:subject (expect-list-ref 2 0)
  (expect-exp-faults '(2))
  (expect-exp-faults '(2 foo bar))
  (expect-exp-faults '(3 foo) expect-any))

(test-case "expect-vector-ref"
  (test-subject #:subject (expect-vector-ref (expect-pred number?) 2)
    (expect-exp-faults #(1 2 3 4 5))
    (expect-exp-faults #(1 2 foo 4 5)
                       (expect-fault
                        #:expected (make-pred-attribute number?)
                        #:actual (make-self-attribute 'foo)
                        #:contexts (list (make-sequence-context 2))))
    (expect-exp-faults #(1 2 3 4 foo)))
  (test-subject "conversion" #:subject (expect-vector-ref '(1 2 3) 1)
    (expect-exp-faults #(foo (1 2 3) bar))
    (expect-exp-faults #(foo (100 2 3) bar)
                       (expect-fault
                        #:contexts (list (make-sequence-context 1)
                                         (make-sequence-context 0)))))
  (test-subject "out-of-bounds" #:subject (expect-vector-ref expect-any 2)
    (expect-exp-apply #(1 2) (expect-raise (expect-exn)))))

(test-case "expect-list-length"
  (test-subject #:subject (expect-list-length (expect-pred even?))
    (expect-exp-faults '(1 2))
    (expect-exp-faults '(1 2 3 4))
    (expect-exp-faults '(1 2 3)
                       (expect-fault #:expected (make-pred-attribute even?)
                                     #:actual (make-self-attribute 3)
                                     #:contexts (list the-length-context)))
    (expect-exp-faults 'foo
                       (expect-fault #:expected (make-pred-attribute list?)
                                     #:actual (make-self-attribute 'foo)
                                     #:contexts (list))))
  (test-subject "conversion" #:subject (expect-list-length 2)
    (expect-exp-faults '(1 2))
    (expect-exp-faults '(1 2 3) expect-any)))

(test-subject "expect-vector-length" #:subject (expect-vector-length 2)
  (expect-exp-faults #(1 2))
  (expect-exp-faults #(1 2 3) expect-any)
  (expect-exp-faults 'foo expect-any))
