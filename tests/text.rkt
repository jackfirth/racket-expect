#lang racket/base

(require expect
         expect/rackunit
         (only-in rackunit test-case))

(test-case "expect-regexp-match"  
  (check-expect "some foo message" (expect-regexp-match #rx"foo"))
  (check-expect "12x4x6" (expect-regexp-match #rx"x." '("x4")))
  (check-expect "12x4x6"
                (expect-regexp-match #rx"x." (list (expect-equal? "x4"))))
  (check-expect "12x4x6" (expect-regexp-match #rx"x." (expect-list "x4")))
  (check-expect (expect-regexp-match #rx"y.")
                (expect-exp-faults "12x4x6" expect-any)))

(test-case "expect-string-contains?"
  (define foo-exp (expect-string-contains? "foo"))
  (check-expect foo-exp (expect-exp-faults "some foo message"))
  (define fault-exp
    (expect-fault #:expected (make-string-contains-attribute "foo")
                  #:actual (make-self-attribute "some bar message")
                  #:contexts (list)))
  (check-expect foo-exp (expect-exp-faults "some bar message" fault-exp))
  (check-expect foo-exp (expect-exp-faults 'not-a-string expect-any)))
