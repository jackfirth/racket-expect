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
  [expect-pred (-> predicate/c expectation?)]
  [expect-true expectation?]
  [expect-false expectation?]
  [expect-not-false expectation?]
  [expect-list (->* () #:rest (listof expectation?) expectation?)]))

(require fancy-app
         racket/format
         racket/function
         "base.rkt"
         "combinator.rkt")


;; Equivalence constructors

(struct eq-attribute attribute (value) #:transparent)
(struct eqv-attribute attribute (value) #:transparent)
(struct equal-attribute attribute (value) #:transparent)

(define (comp-attr construct desc v)
  (construct (format "~a to ~v" desc v) v))

(define (make-eq-attribute v) (comp-attr eq-attribute "eq?" v))
(define (make-eqv-attribute v) (comp-attr eqv-attribute "eqv?" v))
(define (make-equal-attribute v) (comp-attr equal-attribute "equal?" v))

(define (expect-compare comparison attr e)
  (expectation
   (λ (v)
     (if (comparison e v)
         (list)
         (list (fault #:summary "a different value"
                      #:expected (attr e)
                      #:actual (make-self-attribute v)))))))

(define (expect-eq? e) (expect-compare eq? make-eq-attribute e))
(define (expect-eqv? e) (expect-compare eqv? make-eqv-attribute e))
(define (expect-equal? e) (expect-compare equal? make-equal-attribute e))

(struct not-attribute attribute (negated) #:transparent)

(define (make-not-attribute negated)
  (not-attribute (format "not ~a" (attribute-description negated)) negated))

(define ((negate-attribute attr-proc) v)
  (make-not-attribute (attr-proc v)))

(define (expect-not-eq? e)
  (expect-compare (negate eq?) (negate-attribute make-eq-attribute) e))

(define (expect-not-eqv? e)
  (expect-compare (negate eqv?) (negate-attribute make-eqv-attribute) e))

(define (expect-not-equal? e)
  (expect-compare (negate equal?) (negate-attribute make-equal-attribute) e))

(struct =-attribute attribute (value tolerance) #:transparent)

(define (make-=-attribute value tolerance)
  (=-attribute (format "= to ~v (within a tolerance of ~v)" value tolerance)
               value
               tolerance))

(define (expect-= e tolerance)
  (define lower (- e tolerance))
  (define upper (+ e tolerance))
  (expectation
   (λ (v)
     (if (<= lower v upper)
         (list)
         (list (fault #:summary "a different number"
                      #:expected (make-=-attribute e tolerance)
                      #:actual (make-self-attribute v)))))))

;; Predicate and boolean constructors

(struct pred-attribute attribute (pred) #:transparent)

(define (make-pred-attribute pred)
  (pred-attribute (format "value satisfying ~a" (object-name pred)) pred))

(define (expect-pred pred)
  (expectation
   (λ (v)
     (if (pred v)
         (list)
         (list (fault #:summary "a different kind of value"
                      #:expected (make-pred-attribute pred)
                      #:actual (make-self-attribute v)))))))

(define expect-true
  (expectation
   (λ (v)
     (if (equal? v #t)
         (list)
         (list (fault #:summary "true"
                      #:expected (make-self-attribute #t)
                      #:actual (make-self-attribute v)))))))

(define expect-false
  (expectation
   (λ (v)
     (if v
         (list (fault #:summary "false"
                      #:expected (make-self-attribute #f)
                      #:actual (make-self-attribute v)))
         (list)))))

(define expect-not-false
  (expectation
   (λ (v)
     (if v
         (list)
         (list (fault #:summary "a non-false value"
                      #:expected (make-not-attribute
                                  (make-self-attribute #f))
                      #:actual (make-self-attribute v)))))))

;; Compound data constructors

(struct list-item-context context (index) #:transparent)

(define (expect-list-item exp index)
  (define ctxt
    (list-item-context (~a "list item" index #:separator " ") index))
  (expect-if (expect-map (expect/context exp ctxt) (list-ref _ index))
             (λ (vs) (< index (length vs)))))

(struct length-attribute attribute (length) #:transparent)

(define (make-length-attribute n)
  (length-attribute (format "length of ~v" n) n))

(define (expect-count expected-count count-proc items-desc)
  (define (~items v) (~a v items-desc #:separator " "))
  (expectation
   (λ (vs)
     (define count (count-proc vs))
     (define (count-fault summary)
       (fault #:summary summary
              #:expected (make-length-attribute expected-count)
              #:actual (make-length-attribute count)))
     (cond [(< expected-count count) (list (count-fault (~items "fewer")))]
           [(< count expected-count) (list (count-fault (~items "more")))]
           [else (list)]))))

(define (expect-list . exps)
  (define item-exps
    (for/list ([exp (in-list exps)] [i (in-naturals)])
      (expect-list-item exp i)))
  (define count-exp (expect-count (length exps) length "list items"))
  (expect-and (expect-pred list?)
              (apply expect-all count-exp item-exps)))
