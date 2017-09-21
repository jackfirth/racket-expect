#lang racket/base

(require expect
         expect/rackunit
         racket/string
         syntax/parse/define
         (only-in rackunit test-begin test-case))


(define there-exp
  (expect-fault #:expected (expect-pred equal-attribute?)
                #:contexts (list (expect-pred syntax-context?))))

(test-subject "expect-syntax" #:subject (expect-syntax 'here)
  (expect-exp-faults #'here)
  (expect-exp-faults 'here
                     (expect-fault #:expected (expect-pred pred-attribute?)))
  (expect-exp-faults #'there there-exp))

(define-syntax-rule (foo ([id v] ...)) (bar v ...))
(define-syntax-rule (bar v) add1)
(define-namespace-anchor here)
(define here-ns (namespace-anchor->namespace here))

(define expect-expand-raise-any-fault
  (expect-fault #:expected the-none-attribute
                #:contexts (list (make-apply-context expand)
                                 the-raise-context)))

(define expect-expand-once-raise-any-fault
  (expect-fault #:expected the-none-attribute
                #:contexts (list (make-apply-context expand-once)
                                 the-raise-context)))

(test-case "expect-expand"
  (test-subject "not-raise" #:subject (expect-expand expect-not-raise)
    (expect-exp-faults #'(void))
    (expect-exp-faults #'(let ([a 1]) (let (1) (void)))
                       expect-expand-raise-any-fault))
  (test-subject "#:namespace"
    #:subject (expect-expand (expect-return (expect-syntax 'add1))
                             #:namespace here-ns)
    (expect-exp-faults #'(foo ([a 1])))
    (expect-exp-faults
     #'sub1
     (expect-fault #:expected (expect-pred equal-attribute?)
                   #:actual (make-self-attribute 'sub1)
                   #:contexts (list (make-apply-context expand)
                                    the-return*-context
                                    (make-sequence-context 0)
                                    (expect-pred syntax-context?))))
    (expect-exp-faults #'(foo ([a 1] [b 2]))
                       expect-expand-raise-any-fault)))

(test-case "expect-expand-once"
  (test-subject "not-raise" #:subject (expect-expand-once expect-not-raise)
    (expect-exp-faults #'(void))
    (expect-exp-faults #'(let ([a 1]) (let (1) (void))))
    (expect-exp-faults #'(let (1) (void)) expect-expand-once-raise-any-fault))
  (test-subject "#:namespace"
    #:subject (expect-expand-once (expect-return #'(bar 1 2))
                                  #:namespace here-ns)
    (expect-exp-faults #'(foo ([a 1] [b 2])))
    (expect-exp-faults #'(foo (1)) expect-expand-once-raise-any-fault)))

(test-case "expect-syntax-exn"
  (test-subject #:subject (expect-syntax-exn (expect-string-contains? "let"))
    (expect-exp-faults #'(let (1) (void)))
    (expect-exp-faults #'(lambda) expect-any))
  (test-subject "default" #:subject (expect-syntax-exn)
    (expect-exp-faults #'(let (1) (void)))
    (expect-exp-faults #'(let ([a 1]) (void)) expect-any))
  (test-subject "regexp" #:subject (expect-syntax-exn #rx"lamb")
    (expect-exp-faults #'(lambda))
    (expect-exp-faults #'(let (1) (void)) expect-any))
  (test-subject "#:namespace" #:subject (expect-syntax-exn #:namespace here-ns)
    (expect-exp-faults
     #'(foo ([v 1]))
     (expect-fault #:expected the-any-attribute
                   #:actual the-none-attribute
                   #:contexts (list (make-apply-context expand)
                                    the-raise-context)))))
