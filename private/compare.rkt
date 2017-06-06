#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-eq? (-> any/c expectation?)]
  [expect-not-eq? (-> any/c expectation?)]
  [expect-eqv? (-> any/c expectation?)]
  [expect-not-eqv? (-> any/c expectation?)]
  [expect-equal? (-> any/c expectation?)]
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

(require fancy-app
         racket/format
         racket/function
         "base.rkt"
         "logic.rkt")


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
  (expectation
   (λ (v)
     (if (comparison e v)
         (list)
         (list (fault #:summary "a different value"
                      #:expected (attr e)
                      #:actual (self-attribute v)))))))

(define (expect-eq? e) (expect-compare eq? eq-attribute e))
(define (expect-eqv? e) (expect-compare eqv? eqv-attribute e))
(define (expect-equal? e) (expect-compare equal? equal-attribute e))

(define ((negate-attribute attr-proc) v) (not-attribute (attr-proc v)))

(define (expect-not-eq? e)
  (expect-compare (negate eq?) (negate-attribute eq-attribute) e))

(define (expect-not-eqv? e)
  (expect-compare (negate eqv?) (negate-attribute eqv-attribute) e))

(define (expect-not-equal? e)
  (expect-compare (negate equal?) (negate-attribute equal-attribute) e))

(struct =-attribute attribute (value epsilon)
  #:transparent #:omit-define-syntaxes #:constructor-name make-=-attribute)

(define (=-attribute value tolerance)
  (define desc (format "= to ~v (within a tolerance of ~v)" value tolerance))
  (make-=-attribute desc value tolerance))

(define (expect-= e tolerance)
  (define lower (- e tolerance))
  (define upper (+ e tolerance))
  (expectation
   (λ (v)
     (if (<= lower v upper)
         (list)
         (list (fault #:summary "a different number"
                      #:expected (=-attribute e tolerance)
                      #:actual (self-attribute v)))))))
