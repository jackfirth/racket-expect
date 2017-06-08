#lang racket/base

(provide rest->
         take/chop
         map/index)

(require racket/contract
         racket/list)

(module+ test
  (require rackunit))


(define (rest-> arg/c result/c) (->* () #:rest (listof arg/c) result/c))

(define (take/chop vs list-to-match)
  (take vs (min (length vs) (length list-to-match))))

(module+ test
  (check-equal? (take/chop '(1 2 3) '(a b c)) '(1 2 3))
  (check-equal? (take/chop '(1 2 3 4 5) '(a b c)) '(1 2 3))
  (check-equal? (take/chop '(1) '(a b c)) '(1)))

(define (map/index f vs)
  (map f vs (range (length vs))))

(module+ test
  (check-equal? (map/index + '(10 10 10)) '(10 11 12)))
