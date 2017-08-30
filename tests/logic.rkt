#lang racket/base

(require expect
         racket/function
         rackunit)

(check-exn #rx"expected true" (thunk (expect! 'foo expect-true)))

(check-exn #rx"expected false" (thunk (expect! 'foo expect-false)))

(check-exn #rx"expected not false" (thunk (expect! #f expect-not-false)))

(check-exn #rx"expected a different kind of value"
           (thunk (expect! 'foo (expect-pred number?))))
(check-exn #rx"expected: value satisfying number?"
           (thunk (expect! 'foo (expect-pred number?))))

(test-case "expect-all"
  (define all/num+sym? (expect-all (expect-pred number?) (expect-pred symbol?)))
  (check-equal? (length (expectation-apply all/num+sym? "neither")) 2)
  (define and/num+sym? (expect-and (expect-pred number?) (expect-pred symbol?)))
  (check-equal? (length (expectation-apply and/num+sym? "neither")) 1)
  (define pos-num? (expect-and (expect-pred number?) (expect-pred positive?)))
  (check-not-exn (thunk (expect! 4 pos-num?))))
