#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-set-member? (-> any/c expectation?)]
  [expect-set-not-member? (-> any/c expectation?)]))

(require racket/set
         "base.rkt"
         "combinator.rkt"
         "logic.rkt")

(module+ test
  (require racket/function
           rackunit))


(struct member-attribute attribute (value)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-member-attribute)

(define (member-attribute v)
  (make-member-attribute (format "set containing ~v" v) v))

(define (expect-set-member? v)
  (define (make-fault st)
    (and (not (set-member? st v))
         (fault #:summary "a set containing a specific value"
                #:expected (member-attribute v)
                #:actual (self-attribute st))))
  (expect/singular make-fault))

(module+ test
  (check-not-exn (thunk (expect! (set 1 2 3) (expect-set-member? 1))))
  (check-exn #rx"expected a set containing a specific value"
             (thunk (expect! (set 1 2 3) (expect-set-member? 'foo))))
  (check-exn #rx"expected: set containing 'foo"
             (thunk (expect! (set 1 2 3) (expect-set-member? 'foo)))))

(define (expect-set-not-member? v)
  (define (make-fault st)
    (and (set-member? st v)
         (fault #:summary "a set not containing a specific value"
                #:expected (not-attribute (member-attribute v))
                #:actual (self-attribute st))))
  (expect/singular make-fault))

(module+ test
  (check-not-exn (thunk (expect! (set 1 2 3) (expect-set-not-member? 'foo))))
  (check-exn #rx"expected a set not containing a specific value"
             (thunk (expect! (set 1 2 3) (expect-set-not-member? 1))))
  (check-exn #rx"expected: not set containing 1"
             (thunk (expect! (set 1 2 3) (expect-set-not-member? 1)))))
