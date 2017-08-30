#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-any expectation?]
  [expectation (-> (-> any/c (listof fault?)) expectation?)]
  [expectation? predicate/c]
  [expectation-apply (-> expectation? any/c (listof fault?))]
  [fault (->* (#:summary string?
               #:actual attribute?
               #:expected attribute?)
              (#:contexts (listof context?))
              fault?)]
  [fault? predicate/c]
  [fault-summary (-> fault? string?)]
  [fault-actual (-> fault? attribute?)]
  [fault-expected (-> fault? attribute?)]
  [fault-contexts (-> fault? (listof context?))]
  [struct context ([description string?]) #:omit-constructor]
  [struct attribute ([description string?]) #:omit-constructor]
  [self-attribute (-> any/c self-attribute?)]
  [self-attribute? predicate/c]
  [self-attribute-value (-> self-attribute? any/c)]))

(require racket/format
         racket/function
         racket/list
         racket/string)

(module+ test
  (require rackunit))


(struct expectation (proc))
(struct context (description) #:transparent)
(struct attribute (description) #:transparent)

(struct self-attribute attribute (value)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-self-attribute)

(struct fault (summary expected actual contexts)
  #:transparent #:omit-define-syntaxes #:constructor-name make-fault)

(define (fault #:summary summary
               #:expected expected
               #:actual actual
               #:contexts [contexts (list)])
  (make-fault summary expected actual contexts))

(define (self-attribute v) (make-self-attribute (~v v) v))
(define (expectation-apply exp v) ((expectation-proc exp) v))
(define expect-any (expectation (const '())))
