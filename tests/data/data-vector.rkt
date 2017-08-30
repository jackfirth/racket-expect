#lang racket/base

(require expect
         racket/function
         rackunit)


(define expect-num+sym
  (expect-vector (expect-pred number?) (expect-pred symbol?)))
(check-not-exn (thunk (expect! #(10 foo) expect-num+sym)))
(check-exn exn:fail:expect? (thunk (expect! #(10 20) expect-num+sym)))
(check-exn exn:fail:expect? (thunk (expect! #(foo bar) expect-num+sym)))
(check-exn exn:fail:expect? (thunk (expect! #(10) expect-num+sym)))
(check-exn exn:fail:expect? (thunk (expect! #(10 foo extra) expect-num+sym)))
(check-exn exn:fail:expect? (thunk (expect! 'not-list expect-num+sym)))
