#lang racket/base

(require expect
         expect/rackunit
         fancy-app
         racket/function
         racket/set
         (only-in rackunit test-case))


(test-case "expect-hash-count"
  (check-expect (expect-hash-count 2) (expect-exp-faults (hash 'a 1 'b 2))))

(test-case "expect-hash-ref"
  (define foo-ref-exp (expect-hash-ref 'foo 1))
  (check-expect foo-ref-exp (expect-exp-faults (hash 'foo 1)))
  (define ctxt-exp
    (expect-struct context [context-description "value for key 'foo"]))
  (define fault-exp (expect-fault #:contexts (list ctxt-exp)))
  (check-expect foo-ref-exp (expect-exp-faults (hash 'foo 2) fault-exp)))

(test-case "expect-hash-keys"
  (check-expect (expect-hash-keys (set 'foo))
                (expect-exp-faults (hash 'foo 1))))

(test-case "expect-hash"
  (define hash-exp (expect-hash 'foo 1 'bar 2))
  (check-expect hash-exp (expect-exp-faults (hash 'foo 1 'bar 2)))
  (check-expect hash-exp (expect-exp-faults (hash 'foo 1) expect-any))
  (check-expect hash-exp
                (expect-exp-faults (hash 'foo 1 'bar 2 'extra 3) expect-any))
  (check-expect hash-exp (expect-exp-faults (hash 'foo 1 'bar 100) expect-any)))
