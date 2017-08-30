#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-set-member? (-> any/c expectation?)]
  [expect-set-not-member? (-> any/c expectation?)]
  [expect-subset (-> set? expectation?)]
  [expect-superset (-> set? expectation?)]
  [expect-set (rest-> any/c expectation?)]))

(module+ for-conversion
  (provide
   (contract-out
    [expect-set-count (-> expectation? expectation?)])))

(require fancy-app
         racket/set
         expect/private/base
         expect/private/combinator
         expect/private/logic
         "data-collect.rkt"
         expect/private/util)


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

(define (expect-set-not-member? v)
  (define (make-fault st)
    (and (set-member? st v)
         (fault #:summary "a set not containing a specific value"
                #:expected (not-attribute (member-attribute v))
                #:actual (self-attribute st))))
  (expect/singular make-fault))

(define (expect-subset big-st)
  (define (make-expectation-from-extras little-st)
    (apply expect-all
           (map expect-set-not-member?
                (set->list (set-subtract little-st big-st)))))
  (expect/dependent make-expectation-from-extras))

(define (expect-superset little-st)
  (apply expect-all (map expect-set-member? (set->list little-st))))

(define expect-set-count (expect/count _ set-count))

(define (expect-set . vs)
  (define st (list->set vs))
  (expect-all (expect-subset st) (expect-superset st)))
