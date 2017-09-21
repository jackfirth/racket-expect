#lang racket/base

(require expect
         expect/rackunit
         (only-in rackunit test-case))


(test-case "expect-box"
  (test-subject #:subject (expect-box (expect-pred symbol?))
    (expect-exp-faults (box 'foo))
    (expect-exp-faults (box "foo")
                       (expect-fault #:contexts (list the-box-context)))
    (expect-exp-faults 'foo
                       (expect-fault #:expected (make-pred-attribute box?))))
  (test-subject "conversion" #:subject (expect-box 1)
    (expect-exp-faults (box 1))
    (expect-exp-faults (box 2) expect-any)))
