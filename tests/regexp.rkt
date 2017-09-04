#lang racket/base

(require expect
         expect/rackunit)

(check-expect "some foo message" (expect-regexp-match #rx"foo"))
(check-expect "12x4x6" (expect-regexp-match #rx"x." '("x4")))
(check-expect "12x4x6"
              (expect-regexp-match #rx"x." (list (expect-equal? "x4"))))
(check-expect "12x4x6" (expect-regexp-match #rx"x." (expect-list "x4")))
(check-expect (expect-regexp-match #rx"y.") (expect-exp-one-fault "12x4x6"))
(check-expect "12x4x6" (expect-regexp-match #rx"y." #f))
