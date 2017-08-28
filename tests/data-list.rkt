#lang racket/base

(module+ test
  (require expect
           expect/rackunit
           (only-in rackunit test-case))

  (test-case "expect-list"
    (define e (expect-list 1 2))
    (check-expect e (expect-exp-no-faults (list 1 2)))
    (check-expect e (expect-exp-one-fault 'foo))
    (check-expect e (expect-exp-one-fault (list 1 'a)))
    (check-expect e (expect-exp-one-fault (list 1)))
    (check-expect e (expect-exp-faults (list 'a) (expect-list-count 2)))))
