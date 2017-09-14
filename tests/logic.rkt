#lang racket/base

(require expect
         expect/rackunit
         racket/function
         (only-in rackunit define-check test-case))


(define-check (check-fault exp input fault-exp)
  (check-expect exp (expect-exp-faults input fault-exp)))

(check-fault expect-true 'foo (expect-fault #:summary "true"))
(check-fault expect-false 'foo (expect-fault #:summary "false"))
(check-fault expect-not-false #f (expect-fault #:summary "not false"))

(check-fault (expect-pred number?) 'foo
             (expect-fault #:summary "a different kind of value"
                           #:expected (expect-attribute
                                       "value satisfying number?")))

(check-expect (expect-all (expect-pred number?) (expect-pred symbol?))
              (expect-exp-faults* "neither" (expect-list-count 2)))

(check-expect (expect-and (expect-pred number?) (expect-pred symbol?))
              (expect-exp-faults "neither" expect-any))

(check-expect (expect-conjoin number? symbol?)
              (expect-exp-faults "neither" expect-any))

(check-fault (expect-disjoin number? symbol?) "neither"
             (expect-fault #:summary "a different kind of value"
                           #:expected
                           (expect-struct or-attribute
                                          [or-attribute-cases
                                           (expect-list-count 2)])))
