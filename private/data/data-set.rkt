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
         expect/private/util
         "data-collect.rkt"
         (submod "data-list.rkt" for-count))


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
                #:actual (make-self-attribute st))))
  (expectation-rename (expect/singular make-fault) 'set-member?))

(define (expect-set-not-member? v)
  (define (make-fault st)
    (and (set-member? st v)
         (fault #:summary "a set not containing a specific value"
                #:expected (not-attribute (member-attribute v))
                #:actual (make-self-attribute st))))
  (expectation-rename (expect/singular make-fault) 'set-not-member?))

(define (expect-subset big-st)
  (define (make-expectation-from-extras little-st)
    (apply expect-all
           (map expect-set-not-member?
                (set->list (set-subtract little-st big-st)))))
  (expectation-rename (expect/dependent make-expectation-from-extras) 'subset))

(define (expect-superset little-st)
  (define exp
    (apply expect-all (map expect-set-member? (set->list little-st))))
  (expectation-rename exp 'superset))

(define (expect-set-count exp)
  (expectation-rename (expect/count exp set-count) 'set-count))

(define (expect-set . vs)
  (define st (list->set vs))
  (expectation-rename (expect-all (expect-subset st) (expect-superset st))
                      'set))
