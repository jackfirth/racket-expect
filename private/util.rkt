#lang racket/base

(provide rest->
         rest->*
         take/chop
         map/index
         slice)

(require fancy-app
         racket/contract
         racket/list)

(module+ test
  (require rackunit))


(define (rest-> arg/c result/c) (->* () #:rest (listof arg/c) result/c))

(define (rest->* args/c result/c)
  (define (rest/c)
    (define (nonempty) (foldr cons/c (rest/c) args/c))
    (recursive-contract (or/c empty? (nonempty)) #:list-contract?))
  (->* () #:rest (rest/c) result/c))

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

(define (slice vs
               #:start [start 0]
               #:stop [stop (length vs)]
               #:step [step 1])
  (map (list-ref vs _) (range start stop step)))

(module+ test
  (check-equal? (slice '(a b c d e)) '(a b c d e))
  (check-equal? (slice '(a b c d e) #:start 2) '(c d e))
  (check-equal? (slice '(a b c d e) #:stop 2) '(a b))
  (check-equal? (slice '(a b c d e) #:step 2) '(a c e))
  (check-equal? (slice '(a b c d e) #:start 1 #:stop 4 #:step 2) '(b d)))
