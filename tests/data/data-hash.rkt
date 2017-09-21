#lang racket/base

(require arguments
         expect
         expect/rackunit
         fancy-app
         racket/function
         racket/set
         (only-in rackunit test-case))


(test-case "expect-hash-count"
  (test-subject #:subject (expect-hash-count (expect-pred even?))
    (expect-exp-faults (hash 'a 1 'b 2))
    (expect-exp-faults (hash))
    (expect-exp-faults (hash 'a 1 'b 2 'c 3)
                       (expect-fault #:expected (make-pred-attribute even?)
                                     #:actual (make-self-attribute 3)
                                     #:contexts (list the-length-context))))
  (test-subject "conversion" #:subject (expect-hash-count 2)
    (expect-exp-faults (hash 'a 1 'b 2))
    (expect-exp-faults (hash 'a 1) expect-any))
  (test-subject "contract" #:subject expect-hash-count
    (expect-call-exn (arguments 'foo) #rx"contract")))

(test-case "expect-hash-ref"
  (test-subject #:subject (expect-hash-ref 'foo (expect-pred positive?))
    (expect-exp-faults (hash 'foo 10 'bar 2))
    (expect-exp-faults (hash 'foo 10 'bar -3))
    (expect-exp-faults (hash 'foo 10))
    (expect-exp-faults (hash 'foo -10)
                       (expect-fault #:expected (make-pred-attribute positive?)
                                     #:actual (make-self-attribute -10)
                                     #:contexts (list
                                                 (make-dict-context 'foo)))))
  (test-subject "conversion" #:subject (expect-hash-ref 'foo '(1 2 3))
    (expect-exp-faults (hash 'foo '(1 2 3) 'bar '(4 5 6)))
    (expect-exp-faults (hash 'foo '(1 2 10)) expect-any)))

(test-case "expect-hash-keys"
  (define keys-length-contexts (list the-keys-context the-length-context))
  (test-subject #:subject (expect-hash-keys (expect-set-count 2))
    (expect-exp-faults (hash 'a 1 'b 2))
    (expect-exp-faults (hash 'a 1)
                       (expect-fault #:expected (make-equal-attribute 2)
                                     #:actual (make-self-attribute 1)
                                     #:contexts keys-length-contexts)))
  (test-subject "conversion" #:subject (expect-hash-keys (set 'a 'b))
    (expect-exp-faults (hash 'a 1 'b 2))
    (expect-exp-faults (hash 'a 1) expect-any))
  (test-subject "contract" #:subject expect-hash-keys
    (expect-call-exn (arguments '(a b)) #rx"contract")))

(test-case "expect-hash"
  (test-subject #:subject (expect-hash 'a expect-any 'b (expect-equal? 2))
    (expect-exp-faults (hash 'a 1 'b 2))
    (expect-exp-faults (hash 'a 1 'b 100)
                       (expect-fault #:contexts (list (make-dict-context 'b))))
    (expect-exp-faults (hash 'a 100 'b 2))
    (expect-exp-faults (hash 'a 1)
                       (expect-fault
                        #:expected
                        (make-contains-all-attribute set-member? '(b))
                        #:actual (make-self-attribute (set 'a))
                        #:contexts (list the-keys-context)))
    (expect-exp-faults (hash 'a 1 'b 2 'c 3)
                       (expect-fault
                        #:expected
                        (make-contains-none-attribute set-member? '(c))
                        #:actual (make-self-attribute (set 'a 'b 'c))
                        #:contexts (list the-keys-context))))
  (test-subject "conversion" #:subject (expect-hash 'a '(1 2 3))
    (expect-exp-faults (hash 'a '(1 2 3)))
    (expect-exp-faults (hash 'a '(1 2 100))
                       (expect-fault
                        #:contexts (list (make-dict-context 'a)
                                         (make-sequence-context 2)))))
  (test-subject "contract" #:subject expect-hash
    (expect-call-exn (arguments 'a 1 'b) #rx"contract")))
