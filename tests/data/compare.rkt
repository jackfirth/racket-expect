#lang racket/base

(require expect
         racket/function
         rackunit)


(check-exn #rx"expected a different value"
           (thunk (expect! 'foo (expect-eq? 'bar))))
(check-exn #rx"expected: eqv\\? to 'bar"
           (thunk (expect! 'foo (expect-eqv? 'bar))))
(check-exn #rx"actual: 'foo"
           (thunk (expect! 'foo (expect-equal? 'bar))))
(check-not-exn (thunk (expect! 'foo (expect-eq? 'foo))))


(check-exn #rx"expected a different value"
           (thunk (expect! 'foo (expect-not-eq? 'foo))))
(check-exn #rx"expected: not eqv\\? to 'foo"
           (thunk (expect! 'foo (expect-not-eqv? 'foo))))
(check-exn #rx"actual: 'foo"
           (thunk (expect! 'foo (expect-not-equal? 'foo))))
(check-not-exn (thunk (expect! 'foo (expect-not-eq? 'bar))))


(check-exn #rx"expected a different number"
           (thunk (expect! 10 (expect-= 15 0.1))))
(check-exn #rx"expected: = to 15 \\(within a tolerance of 0\\.1\\)"
           (thunk (expect! 10 (expect-= 15 0.1))))
(check-exn #rx"actual: 10"
           (thunk (expect! 10 (expect-= 15 0.1))))
(check-not-exn (thunk (expect! 10 (expect-= 10 0.1))))
(check-not-exn (thunk (expect! 10.0001 (expect-= 10 0.1))))
