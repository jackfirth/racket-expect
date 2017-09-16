#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [struct (dict-context context)
    ([description string?] [key any/c]) #:omit-constructor]
  [make-dict-context (-> any/c dict-context?)]
  [struct (sequence-context context)
    ([description string?] [position exact-nonnegative-integer?])
    #:omit-constructor]
  [make-sequence-context (-> exact-nonnegative-integer? sequence-context?)]))

(require expect/private/lite)

(module+ test
  (require rackunit))


(struct dict-context context (key) #:transparent)
(struct sequence-context context (position) #:transparent)

(define (make-dict-context key)
  (dict-context (format "value for key ~v" key) key))

(module+ test
  (check-equal? (make-dict-context 'foo)
                (dict-context "value for key 'foo" 'foo)))

(define (make-sequence-context position)
  (sequence-context (format "item at position ~v" position) position))

(module+ test
  (check-equal? (make-sequence-context 123)
                (sequence-context "item at position 123" 123)))
