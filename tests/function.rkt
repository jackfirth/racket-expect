#lang racket/base

(require arguments
         expect
         expect/rackunit
         racket/function
         racket/string
         (only-in rackunit test-case)
         "function-util.rkt")

(define (expect-attribute descr)
  (expect-struct attribute [attribute-description descr]))

(define (expect-expects input descr)
  (expect-exp-faults input
                     (expect-fault #:expected (expect-attribute descr))))

(define (raise-foo) (raise 'foo))

(check-expect void expect-not-raise)
(check-expect expect-not-raise (expect-expects raise-foo "nothing"))
(check-expect raise-foo (expect-raise 'foo))
(check-return (expect-raise 'foo) (expect-expects void "anything"))
(check-return (expect-raise 'bar) (expect-expects raise-foo "equal? to 'bar"))
(check-expect (thunk 'foo) (expect-return 'foo))
(check-expect values (expect-return))
(check-expect (thunk (values 'foo 'bar)) (expect-return 'foo 'bar))
(check-return (expect-return 'foo)
              (expect-expects (thunk 'bar) "equal? to 'foo"))
(check-expect (expect-return 'foo)
              (expect-expects identity "arity accepting 0 arguments"))
(check-expect (expect-return 'foo) (expect-expects raise-foo "nothing"))

(define exp-exn-message-context
  (expect-struct struct-accessor-context
                 [struct-accessor-context-accessor-id
                  (expect-syntax 'exn-message)]))

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
      (define bar-exn-attr (equal-attribute "bar exception"))
      (check-expect (expect-exn "bar exception")
                    (exp-foo-exn-expects bar-exn-attr)))
    (test-case "regexp"
      (define foooo-attr (make-regexp-match-attribute #rx"foooo"))
      (check-expect (expect-exn #rx"foooo") (exp-foo-exn-expects foooo-attr)))
    (test-case "expectation"
      (define starts-with-bar-attr (pred-attribute starts-with-bar?))
      (check-expect (expect-exn (expect-pred starts-with-bar?))
                    (exp-foo-exn-expects starts-with-bar-attr)))))

(define foo-args (arguments 'foo))

(test-case "expect-call-exn"
  (check-expect (expect-call-exn foo-args #rx"foo") (expect-exp-faults error))
  (define exp (exp-exn-rx-fault #rx"nonsense" (make-call-context foo-args)))
  (check-expect (expect-call-exn foo-args #rx"nonsense")
                (expect-exp-faults error exp)))

(test-case "expect-apply-exn"
  (check-expect (expect-apply-exn error #rx"foo") (expect-exp-faults foo-args))
  (define exp (exp-exn-rx-fault #rx"nonsense" (make-apply-context error)))
  (check-expect (expect-apply-exn error #rx"nonsense")
                (expect-exp-faults foo-args exp)))
