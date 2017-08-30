#lang racket/base

(require expect
         fancy-app
         racket/function
         rackunit)


(check-not-exn
 (thunk (expect! (hash 'a 1 'b 2) (expect-hash-count (expect-equal? 2)))))

(check-exn #rx"in: value for key 'foo"
           (thunk (expect! (hash 'foo 1)
                           (expect-hash-ref 'foo (expect-equal? 2)))))

(check-not-exn
 (thunk (expect! (hash 'foo 1) (expect-hash-keys (expect-set-member? 'foo)))))
(check-exn #rx"in: the set of hash keys"
           (thunk (expect! (hash 'foo 1)
                           (expect-hash-keys (expect-set-member? 'bar)))))

(define expect-foo1-bar2!
  (expect! _ (expect-hash 'foo (expect-equal? 1) 'bar (expect-equal? 2))))
(check-not-exn  (thunk (expect-foo1-bar2! (hash 'foo 1 'bar 2))))
(check-exn exn:fail:expect?
           (thunk (expect-foo1-bar2! (hash 'foo 1 'bar 2 'baz 3))))
(check-exn exn:fail:expect?
           (thunk (expect-foo1-bar2! (hash 'foo 1))))
(check-exn exn:fail:expect?
           (thunk (expect-foo1-bar2! (hash 'foo 1 'bar 5))))
