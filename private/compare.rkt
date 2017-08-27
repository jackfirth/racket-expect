#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-eq? (-> any/c expectation?)]
  [expect-not-eq? (-> any/c expectation?)]
  [expect-eqv? (-> any/c expectation?)]
  [expect-not-eqv? (-> any/c expectation?)]
  [expect-not-equal? (-> any/c expectation?)]
  [expect-= (-> real? real? expectation?)]
  [eq-attribute? predicate/c]
  [eq-attribute (-> any/c eq-attribute?)]
  [eq-attribute-value (-> eq-attribute? any/c)]
  [eqv-attribute? predicate/c]
  [eqv-attribute (-> any/c eqv-attribute?)]
  [eqv-attribute-value (-> eqv-attribute? any/c)]
  [equal-attribute? predicate/c]
  [equal-attribute (-> any/c equal-attribute?)]
  [equal-attribute-value (-> equal-attribute? any/c)]
  [=-attribute (-> real? real? =-attribute?)]
  [=-attribute? predicate/c]
  [=-attribute-value (-> =-attribute? real?)]
  [=-attribute-epsilon (-> =-attribute? real?)]))

(module+ no-conversion
  (provide (contract-out [expect-equal? (-> any/c expectation?)])))

(require fancy-app
         racket/format
         racket/function
         "base.rkt"
         "combinator.rkt"
         "logic.rkt")

(module+ test
  (require rackunit))


;; Equivalence constructors

(struct eq-attribute attribute (value)
  #:transparent #:omit-define-syntaxes #:constructor-name make-eq-attribute)

(struct eqv-attribute attribute (value)
  #:transparent #:omit-define-syntaxes #:constructor-name make-eqv-attribute)

(struct equal-attribute attribute (value)
  #:transparent #:omit-define-syntaxes #:constructor-name make-equal-attribute)

(define (comp-attr construct desc v)
  (construct (format "~a to ~v" desc v) v))

(define (eq-attribute v) (comp-attr make-eq-attribute "eq?" v))
(define (eqv-attribute v) (comp-attr make-eqv-attribute "eqv?" v))
(define (equal-attribute v) (comp-attr make-equal-attribute "equal?" v))

(define (expect-compare comparison attr e)
  (define (make-fault v)
    (and (not (comparison e v))
         (fault #:summary "a different value"
                #:expected (attr e)
                #:actual (self-attribute v))))
  (expect/singular make-fault))

(define expect-eq? (expect-compare eq? eq-attribute _))
(define expect-eqv? (expect-compare eqv? eqv-attribute _))
(define expect-equal? (expect-compare equal? equal-attribute _))

(module+ test
  (check-exn #rx"expected a different value"
             (thunk (expect! 'foo (expect-eq? 'bar))))
  (check-exn #rx"expected: eqv\\? to 'bar"
             (thunk (expect! 'foo (expect-eqv? 'bar))))
  (check-exn #rx"actual: 'foo"
             (thunk (expect! 'foo (expect-equal? 'bar))))
  (check-not-exn (thunk (expect! 'foo (expect-eq? 'foo)))))

(define ((negate-attribute attr-proc) v) (not-attribute (attr-proc v)))

(define (expect-not-compare comparison attr e)
  (expect-compare (negate comparison) (negate-attribute attr) e))

(define expect-not-eq? (expect-not-compare eq? eq-attribute _))
(define expect-not-eqv? (expect-not-compare eqv? eqv-attribute _))
(define expect-not-equal? (expect-not-compare equal? equal-attribute _))

(module+ test
  (check-exn #rx"expected a different value"
             (thunk (expect! 'foo (expect-not-eq? 'foo))))
  (check-exn #rx"expected: not eqv\\? to 'foo"
             (thunk (expect! 'foo (expect-not-eqv? 'foo))))
  (check-exn #rx"actual: 'foo"
             (thunk (expect! 'foo (expect-not-equal? 'foo))))
  (check-not-exn (thunk (expect! 'foo (expect-not-eq? 'bar)))))

(struct =-attribute attribute (value epsilon)
  #:transparent #:omit-define-syntaxes #:constructor-name make-=-attribute)

(define (=-attribute value tolerance)
  (define desc (format "= to ~v (within a tolerance of ~v)" value tolerance))
  (make-=-attribute desc value tolerance))

(define (expect-= e tolerance)
  (define lower (- e tolerance))
  (define upper (+ e tolerance))
  (define (make-fault v)
    (and (not (<= lower v upper))
         (fault #:summary "a different number"
                #:expected (=-attribute e tolerance)
                #:actual (self-attribute v))))
  (expect/singular make-fault))

(module+ test
  (check-exn #rx"expected a different number"
             (thunk (expect! 10 (expect-= 15 0.1))))
  (check-exn #rx"expected: = to 15 \\(within a tolerance of 0\\.1\\)"
             (thunk (expect! 10 (expect-= 15 0.1))))
  (check-exn #rx"actual: 10"
             (thunk (expect! 10 (expect-= 15 0.1))))
  (check-not-exn (thunk (expect! 10 (expect-= 10 0.1))))
  (check-not-exn (thunk (expect! 10.0001 (expect-= 10 0.1)))))
