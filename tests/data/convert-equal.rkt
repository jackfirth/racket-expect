#lang racket/base

(require expect
         expect/rackunit
         (only-in rackunit test-case))


(define b1 (box 'foo))
(define b2 (box 'foo))

(test-case "box"
  (check-expect (expect-equal? b1) (expect-exp-faults b2)))

(test-case "syntax"
  (define id-stx #'foo)
  (check-expect (expect-equal? id-stx) (expect-exp-faults id-stx))
  
  (test-case "wrong-contents"
    (define not-here-exp
      (expect-fault #:expected (make-equal-attribute 'here)
                    #:actual (make-self-attribute 'there)
                    #:contexts (list (expect-pred syntax-context?))))
    (check-expect (expect-equal? #'here)
                  (expect-exp-faults #'there not-here-exp)))

  (test-case "not-eq"
    (define other-id #'foo)
    (define not-eq-exp
      (expect-fault #:expected (make-eq-attribute id-stx)
                    #:actual (make-self-attribute other-id)
                    #:contexts (list)))
    (check-expect (expect-equal? id-stx)
                  (expect-exp-faults other-id not-eq-exp)))

  (define list-stx #'(a b c))
  (define list*-stx #'(a b . c))

  (test-case "list-contents"
    (define list-exp (expect-equal? list-stx))
    (check-expect list-exp (expect-exp-faults list-stx))
    (define fault-exp
      (expect-fault #:expected (make-not-attribute (make-self-attribute #f))
                    #:actual (make-self-attribute #f)
                    #:contexts (list (expect-pred syntax-context?))))
    (check-expect list-exp (expect-exp-faults list*-stx fault-exp)))

  (test-case "list*-contents"
    (define list*-exp (expect-equal? list*-stx))
    (check-expect list*-exp (expect-exp-faults list*-stx))
    (define fault-exp
      (expect-fault #:expected (make-equal-attribute (syntax-e list*-stx))
                    #:actual (make-self-attribute (syntax-e list-stx))
                    #:contexts (list (expect-pred syntax-context?))))
    (check-expect list*-exp (expect-exp-faults list-stx fault-exp))))
