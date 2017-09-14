#lang racket/base

(require expect
         expect/rackunit
         racket/string
         syntax/parse/define
         (only-in rackunit test-begin test-case))


(define-simple-macro (test-begin/expect subject:expr exp:expr ...)
  (test-begin
    (define s subject)
    (check-expect s exp) ...))

(define-simple-macro (test-case/expect name:str subject:expr exp:expr ...)
  (test-case name
    (define s subject)
    (check-expect s exp) ...))

(test-case/expect "expect-syntax"
  (expect-syntax 'here)
  (expect-exp-faults #'here (list))
  (expect-exp-faults
   'here (list (expect-fault #:expected (expect-pred pred-attribute?))))
  (expect-exp-faults
   #'there (list (expect-fault #:expected (expect-pred equal-attribute?)
                               #:contexts (list the-datum-context)))))

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
  (test-case/expect "not-raise"
    (expect-expand expect-not-raise)
    (expect-exp-faults #'(void) (list))
    (expect-exp-faults #'(let ([a 1]) (let (1) (void)))
                       (list expect-expand-raise-any-fault)))
  (test-case/expect "#:namespace"
    (expect-expand (expect-return (expect-syntax 'add1)) #:namespace here-ns)
    (expect-exp-faults #'(foo ([a 1])) (list))
    (expect-exp-faults
     #'sub1
     (list (expect-fault #:expected (expect-pred equal-attribute?)
                         #:actual (make-self-attribute 'sub1)
                         #:contexts (list (make-apply-context expand)
                                          the-return-context
                                          expect-any
                                          the-datum-context))))
    (expect-exp-faults #'(foo ([a 1] [b 2]))
                       (list expect-expand-raise-any-fault))))

(test-case "expect-expand-once"
  (test-case/expect "not-raise"
    (expect-expand-once expect-not-raise)
    (expect-exp-faults #'(void) (list))
    (expect-exp-faults #'(let ([a 1]) (let (1) (void))) (list))
    (expect-exp-faults #'(let (1) (void))
                       (list expect-expand-once-raise-any-fault)))
  (test-case/expect "#:namespace"
    (expect-expand-once (expect-return (expect-syntax '(bar 1 2)))
                        #:namespace here-ns)
    (expect-exp-faults #'(foo ([a 1] [b 2])) (list))
    (expect-exp-faults #'(foo (1)) (list expect-expand-once-raise-any-fault))))

(define (contains-let? str) (string-contains? str "let"))

(test-case "expect-syntax-exn"
  (test-begin/expect
    (expect-syntax-exn (expect-pred contains-let?))
    (expect-exp-faults #'(let (1) (void)) (list))
    (expect-exp-faults #'(lambda) (list (expect-fault))))
  (test-case/expect "default"
    (expect-syntax-exn)
    (expect-exp-faults #'(let (1) (void)) (list))
    (expect-exp-faults #'(let ([a 1]) (void)) (list (expect-fault))))
  (test-case/expect "regexp"
    (expect-syntax-exn #rx"lamb")
    (expect-exp-faults #'(lambda) (list))
    (expect-exp-faults #'(let (1) (void)) (list (expect-fault))))
  (test-case/expect "#:namespace"
    (expect-syntax-exn #:namespace here-ns)
    (expect-exp-faults
     #'(foo ([v 1]))
     (list (expect-fault #:expected the-any-attribute
                         #:actual the-none-attribute
                         #:contexts (list (make-apply-context expand)
                                          the-raise-context))))))
