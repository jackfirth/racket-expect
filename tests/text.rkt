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

(test-subject "expect-string-contains?"
  #:subject (expect-string-contains? "foo")
  (expect-exp-faults "some foo message")
  (expect-exp-faults "some bar message" expect-any)
  (expect-exp-faults 'not-a-string expect-any))
