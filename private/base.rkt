#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-any expectation?]
  [expectation (->* ((-> any/c (listof fault?)))
                    (#:name (or/c symbol? #f))
                    expectation?)]
  [expectation? predicate/c]
  [expectation-apply (-> expectation? any/c (listof fault?))]
  [expectation-rename (-> expectation? (or/c symbol? #f) expectation?)]
  [rename make-fault fault
          (->* (#:summary string?
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
  [struct (self-attribute attribute) ([description string?] [value any/c])
    #:omit-constructor]
  [make-self-attribute (-> any/c self-attribute?)]
  [struct (any-attribute attribute) ([description string?]) #:omit-constructor]
  [the-any-attribute any-attribute?]
  [struct (none-attribute attribute) ([description string?]) #:omit-constructor]
  [the-none-attribute none-attribute?]))

(module+ for-meta
  (provide (struct-out fault)))

(require (for-syntax racket/base
                     syntax/parse/lib/function-header)
         racket/format
         racket/function
         racket/list
         racket/string
         syntax/parse/define)

(module+ test
  (require rackunit))


(define (expectation-print exp port mode)
  (define n (object-name exp))
  (if n
      (fprintf port "#<expectation:~a>" n)
      (fprintf port "#<expectation>")))

(struct expectation (proc name)
  #:omit-define-syntaxes
  #:constructor-name make-expectation
  #:property prop:object-name (struct-field-index name)
  #:methods gen:custom-write
  [(define write-proc expectation-print)])

(define (expectation proc #:name [name #f]) (make-expectation proc name))

(define (expectation-rename exp name)
  (make-expectation (expectation-proc exp) name))

(struct context (description) #:transparent)
(struct attribute (description) #:transparent)

(struct self-attribute attribute (value) #:transparent)
(define (make-self-attribute v) (self-attribute (~v v) v))
(struct any-attribute attribute () #:transparent)
(define the-any-attribute (any-attribute "anything"))
(struct none-attribute attribute () #:transparent)
(define the-none-attribute (none-attribute "nothing"))

(struct fault (summary expected actual contexts) #:transparent)

(define (make-fault #:summary summary
                    #:expected expected
                    #:actual actual
                    #:contexts [contexts (list)])
  (fault summary expected actual contexts))

(define (expectation-apply exp v) ((expectation-proc exp) v))
(define expect-any (expectation-rename (expectation (const '())) 'any))

(module+ test
  (check-equal? (object-name expect-any) 'any)
  (check-equal? (~a expect-any) "#<expectation:any>")
  (check-equal? (~v expect-any) (~a expect-any))
  (check-equal? (~s expect-any) (~a expect-any)))
