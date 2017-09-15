#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [struct (member-attribute attribute)
    ([description string?] [value any/c]) #:omit-constructor]
  [make-member-attribute (-> any/c member-attribute?)]))

(require expect/private/base)

(module+ test
  (require rackunit))


(struct member-attribute attribute (value) #:transparent)

(define (make-member-attribute v)
  (member-attribute (format "set containing ~v" v) v))

(module+ test
  (check-equal? (make-member-attribute 'foo)
                (member-attribute "set containing 'foo" 'foo)))
