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
  [struct (splice-context context)
    ([description string?] [values (listof context?)]) #:omit-constructor]
  [make-splice-context (->* ((listof context?))
                            (#:description string?)
                            splice-context?)]
  [struct attribute ([description string?]) #:omit-constructor]
  [struct (self-attribute attribute) ([description string?] [value any/c])
    #:omit-constructor]
  [make-self-attribute (-> any/c self-attribute?)]
  [the-any-attribute attribute?]
  [the-none-attribute attribute?]))

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

(struct splice-context context (values) #:transparent)
(define (make-splice-context vs #:description [desc* #f])
  (define desc (or desc* (string-join (map context-description vs) " of ")))
  (splice-context desc vs))

(module+ test
  (struct test-context context () #:transparent)
  (define ctxts
    (list (test-context "foo") (test-context "bar") (test-context "baz")))
  (check-equal? (context-description (make-splice-context ctxts))
                "foo of bar of baz")
  (check-equal? (context-description
                 (make-splice-context ctxts #:description "foo.bar.baz"))
                "foo.bar.baz"))

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
  (test-case "named-expectation"
    (check-equal? (object-name expect-any) 'any)
    (check-equal? (~a expect-any) "#<expectation:any>")
    (check-equal? (~v expect-any) (~a expect-any))
    (check-equal? (~s expect-any) (~a expect-any)))
  (test-case "anonymous-expectation"
    (define anon (expectation (const '())))
    (check-equal? (object-name anon) #f)
    (check-equal? (~a anon) "#<expectation>")))
