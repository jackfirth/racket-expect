#lang racket/base

(module+ test
  (require racket/function
           racket/set
           rackunit
           "base.rkt"
           "convert.rkt")

  (check-not-exn (thunk (expect! '(1 2 3) (expect-list 1 2 3))))
  (check-not-exn (thunk (expect! '(foo bar) (expect-list-ref 'foo 0))))
  (check-not-exn (thunk (expect! (hash 'a 1 'b 2) (expect-hash 'a 1 'b 2))))
  (check-exn #rx"multiple failures"
             (thunk (expect! (hash 'a (list 1 'WRONG 3)
                                   'b (vector "foo" "bar")
                                   'c (set 'a 'b 'WRONG)
                                   'd 'WRONG)
                             (expect-equal? (hash 'a (list 1 2 3)
                                                  'b (vector "foo" "bar")
                                                  'c (set 'a 'b 'c)
                                                  'd #f))))))
