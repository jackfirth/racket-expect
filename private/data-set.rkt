#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-set-member? (-> any/c expectation?)]
  [expect-set-not-member? (-> any/c expectation?)]
  [expect-subset (-> set? expectation?)]
  [expect-superset (-> set? expectation?)]
  [expect-set-count (-> expectation? expectation?)]))

(require fancy-app
         racket/set
         "base.rkt"
         "combinator.rkt"
         "data-collect.rkt"
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

(define (expect-subset big-st)
  (define (make-expectation-from-extras little-st)
    (apply expect-all
           (map expect-set-not-member?
                (set->list (set-subtract little-st big-st)))))
  (expect/dependent make-expectation-from-extras))

(module+ test
  (check-not-exn (thunk (expect! (set 1 2) (expect-subset (set 1 2 3)))))
  (check-exn #rx"multiple failures"
             (thunk (expect! (set 1 2 'foo 'bar) (expect-subset (set 1 2 3))))))

(define (expect-superset little-st)
  (apply expect-all (map expect-set-member? (set->list little-st))))

(module+ test
  (check-not-exn (thunk (expect! (set 1 2 3) (expect-superset (set 1 2)))))
  (check-exn #rx"multiple failures"
             (thunk (expect! (set 1) (expect-superset (set 1 2 3))))))

(define expect-set-count (expect/count _ set-count))
