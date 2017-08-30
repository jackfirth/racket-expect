#lang racket/base

(require expect
         fancy-app
         racket/function
         racket/set
         rackunit)


(check-not-exn (thunk (expect! (set 1 2 3) (expect-set-member? 1))))
(check-exn #rx"expected a set containing a specific value"
           (thunk (expect! (set 1 2 3) (expect-set-member? 'foo))))
(check-exn #rx"expected: set containing 'foo"
           (thunk (expect! (set 1 2 3) (expect-set-member? 'foo))))

(check-not-exn (thunk (expect! (set 1 2 3) (expect-set-not-member? 'foo))))
(check-exn #rx"expected a set not containing a specific value"
           (thunk (expect! (set 1 2 3) (expect-set-not-member? 1))))
(check-exn #rx"expected: not set containing 1"
           (thunk (expect! (set 1 2 3) (expect-set-not-member? 1))))

(check-not-exn (thunk (expect! (set 1 2) (expect-subset (set 1 2 3)))))
(check-exn #rx"multiple failures"
           (thunk (expect! (set 1 2 'foo 'bar) (expect-subset (set 1 2 3)))))

(check-not-exn (thunk (expect! (set 1 2 3) (expect-superset (set 1 2)))))
(check-exn #rx"multiple failures"
           (thunk (expect! (set 1) (expect-superset (set 1 2 3)))))

(check-not-exn
 (thunk (expect! (set 1 2) (expect-set-count (expect-pred even?)))))

(check-not-exn (thunk (expect! (set 1 2 3) (expect-set 1 2 3))))
(check-exn #rx"multiple failures"
           (thunk (expect! (set 1 'foo) (expect-set 1 2 3))))
