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

(module+ for-conversion
  (provide (contract-out [expect-equal? (-> any/c expectation?)])))

(require fancy-app
         racket/format
         racket/function
         expect/private/base
         expect/private/combinator
         expect/private/logic
         syntax/parse/define)

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

(define (expect-compare/no-rename comparison attr e)
  (define (make-fault v)
    (and (not (comparison e v))
         (fault #:summary "a different value"
                #:expected (attr e)
                #:actual (make-self-attribute v))))
  (expect/singular make-fault))

(define (expect-compare comparison attr e)
  (expectation-rename (expect-compare/no-rename comparison attr e)
                      (object-name comparison)))

(define (expect-eq? v) (expect-compare eq? eq-attribute v))
(define (expect-eqv? v) (expect-compare eqv? eqv-attribute v))
(define (expect-equal? v) (expect-compare equal? equal-attribute v))

(define ((negate-attribute attr-proc) v) (not-attribute (attr-proc v)))

(define (expect-not-compare comparison attr e)
  (define name (string->symbol (format "not-~a" (object-name comparison))))
  (expectation-rename (expect-compare/no-rename (negate comparison)
                                                (negate-attribute attr)
                                                e)
                      name))

(define (expect-not-eq? v) (expect-not-compare eq? eq-attribute v))
(define (expect-not-eqv? v) (expect-not-compare eqv? eqv-attribute v))
(define (expect-not-equal? v) (expect-not-compare equal? equal-attribute v))

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
                #:actual (make-self-attribute v))))
  (expectation-rename (expect/singular make-fault) '=))
