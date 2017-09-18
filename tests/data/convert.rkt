#lang racket/base

(require racket/function
         racket/set
         expect
         expect/rackunit)


(check-expect '(1 2 3) (expect-list 1 2 3))
(check-expect '(foo bar) (expect-list-ref 'foo 0))
(check-expect (hash 'a 1 'b 2) (expect-hash 'a 1 'b 2))
(check-expect (hash 'a 1 'b 2) (expect-hash-ref 'a 1))
(check-expect (vector 'a 'b) (expect-vector 'a 'b))
(check-expect (vector 'a 'b) (expect-vector-ref 'a 0))
(check-expect (set 'a 'b 'c) (expect-set 'a 'b 'c))

(define compound-data
  (hash 'a (list 1 2 3)
        'b (vector "foo" "bar")
        'c (set 'a 'b 'c)
        'd #f))

(define wrong-data
  (hash 'a (list 1 'WRONG 3)
        'b (vector "foo" "bar")
        'c (set 'a 'b)
        'd 'WRONG))

(define exp-wrong-data
  (expect-exp-faults* wrong-data (expect-list-length 3)))

(check-expect (->expectation compound-data) exp-wrong-data)
(check-expect (expect-equal? compound-data) exp-wrong-data)

(define compound-data/pred-exp
  (hash 'a (list 1 2 3)
        'b (vector (expect-pred string?) "bar")
        'c (set 'a 'b 'c)
        'd #f))

(define exp-wrong-data/pred-fault
  (expect-exp-faults* wrong-data (expect-list-length 4)))

(check-expect (->expectation compound-data/pred-exp) exp-wrong-data)
(check-expect (expect-equal? compound-data/pred-exp)
              exp-wrong-data/pred-fault)
