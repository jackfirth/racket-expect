#lang racket/base

(require expect
         expect/rackunit
         racket/function
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

(test-case "expect-output"
  (test-subject #:subject (expect-output (expect-string-contains? "foo"))
    (expect-exp-faults (thunk (display "foo")))
    (expect-exp-faults (thunk (display "contains foo in output")))
    (expect-exp-faults (thunk (display "bar"))
                       (expect-fault #:actual (make-self-attribute "bar")
                                     #:contexts (list the-output-context)))
    (expect-exp-faults void
                       (expect-fault #:actual (make-self-attribute "")
                                     #:contexts (list the-output-context))))
  (test-case "conversion"
    (test-subject "string" #:subject (expect-output "foo")
      (expect-exp-faults (thunk (display "foo")))
      (expect-exp-faults (thunk (display "contains foo in output")) expect-any)
      (expect-exp-faults (thunk (display "bar")) expect-any)
      (expect-exp-faults void expect-any))
    (test-subject "regexp" #:subject (expect-output #rx"foo.")
      (expect-exp-faults (thunk (display "foos")))
      (expect-exp-faults (thunk (display "foo")) expect-any)
      (expect-exp-faults void expect-any)))
  (test-subject "#:call"
    #:subject (expect-output "foo" #:call (expect-return (void)))
    (expect-exp-faults (thunk (display "foo")))
    (expect-exp-faults (thunk (display "foo") 'value)
                       (expect-fault #:actual (make-self-attribute 'value)
                                     #:contexts (list the-return-context)))
    (expect-exp-faults void
                       (expect-fault #:contexts (list the-output-context)))))
