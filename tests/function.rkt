#lang racket/base

(require arguments
         expect
         expect/rackunit
         racket/function
         racket/string
         (only-in rackunit test-case))

(define (raise-foo) (raise 'foo))
(define (raise-bar) (raise 'bar))

(define (expect-raise-fault exp act)
  (expect-fault #:expected exp
                #:actual act
                #:contexts (list the-raise-context)))

(test-subject "expect-raise" #:subject (expect-raise 'foo)
  (expect-exp-faults raise-foo)
  (expect-exp-faults void
                     (expect-raise-fault the-any-attribute the-none-attribute))
  (expect-exp-faults raise-bar
                     (expect-raise-fault (make-equal-attribute 'foo)
                                         (make-self-attribute 'bar))))

(test-subject "expect-not-raise" #:subject expect-not-raise
  (expect-exp-faults void)
  (expect-exp-faults raise-foo
                     (expect-raise-fault the-none-attribute
                                         (make-self-attribute 'foo))))

(test-case "expect-return"
  (define foo-exp (expect-return 'foo))
  (define bar-fault-exp
    (expect-fault #:expected (make-equal-attribute 'foo)
                  #:actual (make-self-attribute 'bar)
                  #:contexts (list the-return-context
                                   (make-sequence-context 0))))
  (test-subject #:subject foo-exp
    (expect-exp-faults (thunk 'foo))
    (expect-exp-faults (thunk 'bar) bar-fault-exp))
  (test-case "exception"
    (define exn-fault-exp
      (expect-raise-fault the-none-attribute (make-self-attribute 'foo)))
    (check-expect foo-exp (expect-exp-faults raise-foo exn-fault-exp)))
  (test-subject "multiple-values" #:subject (expect-return 'foo 'bar)
    (expect-exp-faults (thunk (values 'foo 'bar))))
  (test-case "no-values"
    (check-expect (expect-return) (expect-exp-faults values)))
  (test-case "not-thunk"
    (define arity-fault-exp
      (expect-fault #:expected (make-arity-includes-attribute 0)
                    #:actual (make-self-attribute 1)
                    #:contexts (list the-arity-context)))
    (check-expect foo-exp (expect-exp-faults add1 arity-fault-exp))))

(define even-fault-exp
  (expect-fault #:expected (make-pred-attribute even?)
                #:actual (make-self-attribute 1)
                #:contexts (list the-return-context the-length-context)))

(test-subject "expect-return*"
  #:subject (expect-return* (expect-list-length (expect-pred even?)))
  (expect-exp-faults values)
  (expect-exp-faults (thunk (values 'foo 'bar)))
  (expect-exp-faults (thunk 'foo) even-fault-exp))

(define exp-exn-message-context
  (expect-struct struct-accessor-context
                 [struct-accessor-context-accessor-id #'exn-message]))

(define (exp-exn-rx-fault pattern ctxt)
  (define ctxt-list (list ctxt the-raise-context exp-exn-message-context))
  (expect-fault #:expected (make-regexp-match-attribute pattern)
                #:actual (expect-pred self-attribute?)
                #:contexts ctxt-list))

(test-case "expect-exn"
  (define foo-exn (make-exn "foo exception" (current-continuation-marks)))
  (define (starts-with-foo? str) (string-prefix? str "foo"))
  (define (starts-with-bar? str) (string-prefix? str "bar"))

  (test-case "passing-input"
    (define foo-exn-passes (expect-exp-faults foo-exn))
    (check-expect (expect-exn "foo exception") foo-exn-passes)
    (check-expect (expect-exn #rx"foo") foo-exn-passes)
    (check-expect (expect-exn (expect-pred starts-with-foo?)) foo-exn-passes))

  (test-case "failing-input"
    (define (exp-foo-exn-expects attr)
      (define fault-exp
        (expect-fault #:expected attr
                      #:actual (make-self-attribute "foo exception")
                      #:contexts (list exp-exn-message-context)))
      (expect-exp-faults foo-exn fault-exp))

    (test-case "string"
      (define bar-exn-attr (make-equal-attribute "bar exception"))
      (check-expect (expect-exn "bar exception")
                    (exp-foo-exn-expects bar-exn-attr)))
    (test-case "regexp"
      (define foooo-attr (make-regexp-match-attribute #rx"foooo"))
      (check-expect (expect-exn #rx"foooo") (exp-foo-exn-expects foooo-attr)))
    (test-case "expectation"
      (define starts-with-bar-attr (make-pred-attribute starts-with-bar?))
      (check-expect (expect-exn (expect-pred starts-with-bar?))
                    (exp-foo-exn-expects starts-with-bar-attr))))

  (test-case "default"
    (check-expect (expect-exn) (expect-exp-faults foo-exn))
    (define fault-exp
      (expect-fault #:actual (make-self-attribute 'not-an-exn)
                    #:expected (make-pred-attribute exn?)
                    #:contexts (list)))
    (check-expect (expect-exn) (expect-exp-faults 'not-an-exn fault-exp))))

(define foo-args (arguments 'foo))

(test-case "expect-call-exn"
  (check-expect (expect-call-exn foo-args #rx"foo") (expect-exp-faults error))
  (define exp (exp-exn-rx-fault #rx"nonsense" (make-call-context foo-args)))
  (check-expect (expect-call-exn foo-args #rx"nonsense")
                (expect-exp-faults error exp))
  (test-case "default"
    (check-expect (expect-call-exn foo-args) (expect-exp-faults error))))

(test-case "expect-apply-exn"
  (check-expect (expect-apply-exn error #rx"foo") (expect-exp-faults foo-args))
  (define exp (exp-exn-rx-fault #rx"nonsense" (make-apply-context error)))
  (check-expect (expect-apply-exn error #rx"nonsense")
                (expect-exp-faults foo-args exp))
  (test-case "default"
    (check-expect (expect-apply-exn error) (expect-exp-faults foo-args))))
