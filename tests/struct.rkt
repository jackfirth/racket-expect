#lang racket/base

(require expect
         expect/rackunit
         racket/function
         (only-in rackunit test-case))

(struct fish (color weight))

(test-case "expect-struct"
  (define red-fish-passes (expect-exp-no-faults (fish 'red 5)))
  (check-expect (expect-struct fish [color 'red] [weight 5]) red-fish-passes)
  (test-case "field-order"
    (check-expect (expect-struct fish [weight 5] [color 'red]) red-fish-passes))
  (test-case "field-optional"
    (check-expect (expect-struct fish [color 'red]) red-fish-passes))
  (test-case "pred"
    (check-expect (expect-struct fish) (expect-exp-one-fault 5))))
