#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-true expectation?]
  [expect-false expectation?]
  [expect-not-false expectation?]
  [expect-pred (-> predicate/c expectation?)]
  [expect-all (rest-> expectation? expectation?)]
  [expect-and (rest-> expectation? expectation?)]
  [not-attribute? predicate/c]
  [not-attribute (-> attribute? not-attribute?)]
  [not-attribute-negated (-> not-attribute? attribute?)]
  [pred-attribute (-> predicate/c pred-attribute?)]
  [pred-attribute? predicate/c]
  [pred-attribute-value (-> pred-attribute? predicate/c)]))

(require fancy-app
         racket/function
         racket/list
         racket/stream
         "base.rkt"
         "combinator.rkt"
         "util.rkt")

(module+ test
  (require rackunit))

;; Booleans

(define (true-fault v)
  (fault #:summary "true"
         #:expected (self-attribute #t)
         #:actual (self-attribute v)))

(define expect-true
  (expect/singular (λ (v) (and (not (equal? v #t)) (true-fault v)))))

(module+ test
  (check-exn #rx"expected true" (thunk (expect! 'foo expect-true))))

(define (false-fault v)
  (fault #:summary "false"
         #:expected (self-attribute #f)
         #:actual (self-attribute v)))

(define expect-false
  (expect/singular (λ (v) (and v (false-fault v)))))

(module+ test
  (check-exn #rx"expected false" (thunk (expect! 'foo expect-false))))

(struct not-attribute attribute (negated)
  #:transparent #:omit-define-syntaxes #:constructor-name make-not-attribute)

(define (not-attribute negated)
  (make-not-attribute (format "not ~a" (attribute-description negated)) negated))

(define (not-false-fault v)
  (fault #:summary "not false"
         #:expected (not-attribute (self-attribute #f))
         #:actual (self-attribute v)))

(define expect-not-false
  (expect/singular (λ (v) (and (not v) (not-false-fault v)))))

(module+ test
  (check-exn #rx"expected not false" (thunk (expect! #f expect-not-false))))

;; Logical / predicate combinators

(struct pred-attribute attribute (value)
  #:transparent #:omit-define-syntaxes #:constructor-name make-pred-attribute)

(define (pred-attribute pred)
  (make-pred-attribute (format "value satisfying ~a" (object-name pred)) pred))

(define (pred-fault pred v)
  (fault #:summary "a different kind of value"
         #:expected (pred-attribute pred)
         #:actual (self-attribute v)))

(define (expect-pred pred)
  (expect/singular (λ (v) (and (not (pred v)) (pred-fault pred v)))))

(module+ test
  (check-exn #rx"expected a different kind of value"
             (thunk (expect! 'foo (expect-pred number?))))
  (check-exn #rx"expected: value satisfying number?"
             (thunk (expect! 'foo (expect-pred number?)))))

(define (expect-all . exps)
  (expectation (λ (v) (append-map (expectation-apply _ v) exps))))

(define (expect-and . exps)
  (expectation
   (λ (v)
     (define faults-stream (stream-map (expectation-apply _ v) exps))
     (or (for/first ([faults (in-stream faults-stream)]
                     #:unless (empty? faults))
           faults)
         (list)))))

(module+ test
  (define all/num+sym? (expect-all (expect-pred number?) (expect-pred symbol?)))
  (check-equal? (length (expectation-apply all/num+sym? "neither")) 2)
  (define and/num+sym? (expect-and (expect-pred number?) (expect-pred symbol?)))
  (check-equal? (length (expectation-apply and/num+sym? "neither")) 1)
  (define pos-num? (expect-and (expect-pred number?) (expect-pred positive?)))
  (check-not-exn (thunk (expect! 4 pos-num?))))
