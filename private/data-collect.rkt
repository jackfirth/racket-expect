#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [count-context count-context?]
  [count-context? predicate/c]
  [index-context (-> exact-nonnegative-integer? index-context?)]
  [index-context? predicate/c]
  [index-context-value (-> index-context? exact-nonnegative-integer?)]))

(require fancy-app
         "base.rkt")

(module+ test
  (require rackunit))


(struct count-context context ()
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-count-context)

(define count-context (make-count-context "the count of items"))

(struct index-context context (value)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-index-context)

(define (index-context i)
  (make-index-context (format "item at position ~v" i) i))

(module+ test
  (check-equal? (index-context 4) (make-index-context "item at position 4" 4)))