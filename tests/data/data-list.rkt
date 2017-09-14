#lang racket/base

(require expect
         expect/rackunit
         (only-in rackunit test-case))

(test-case "expect-list"
  (define e (expect-list 1 2))
  (check-expect e (expect-exp-faults (list 1 2)))
  (check-expect e (expect-exp-faults 'foo expect-any))
  (check-expect e (expect-exp-faults (list 1 'a) expect-any))
  (check-expect e (expect-exp-faults (list 1) expect-any))
  (check-expect e (expect-exp-faults* (list 'a) (expect-list-count 2))))
