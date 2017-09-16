#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-regexp-match
   (->* (regexp?)
        ((or/c (listof (or/c string? bytes? #f expectation?))
               expectation?))
        expectation?)]
  [expect-string-contains? (-> string? expectation?)]
  [struct (string-contains-attribute attribute)
    ([description string?] [value string?]) #:omit-constructor]
  [make-string-contains-attribute (-> string? string-contains-attribute?)]
  [struct (regexp-match-context context)
    ([description string?] [regexp regexp?]) #:omit-constructor]
  [make-regexp-match-context (-> regexp? regexp-match-context?)]
  [struct (regexp-match-attribute attribute)
    ([description string?] [regexp regexp?]) #:omit-constructor]
  [make-regexp-match-attribute (-> regexp? regexp-match-attribute?)]))

(require fancy-app
         racket/string
         "base.rkt"
         "combinator.rkt"
         "data.rkt"
         "logic.rkt")


(define (expect-regexp-match pattern [result-exp expect-any])
  (define anon-exp
    (expect-and (expect-disjoin string? bytes? path? input-port?)
                (expect-regexp-match? pattern)
                (expect/context (expect/proc (->expectation result-exp)
                                             (regexp-match pattern _))
                                (make-regexp-match-context pattern))))
  (expectation-rename anon-exp 'regexp-match))

;; not provided because calling expect-regexp-match with a default result-exp
;; is equivalent to calling this
(define (expect-regexp-match? pattern)
  (expect/singular
   (λ (str)
     (and (not (regexp-match? pattern str))
          (fault #:summary "a value matching a regexp"
                 #:expected (make-regexp-match-attribute pattern)
                 #:actual (make-self-attribute str))))))

(struct regexp-match-context context (regexp) #:transparent)
(define (make-regexp-match-context regexp)
  (regexp-match-context (format "the results of matching ~v" regexp)
                        regexp))

(struct regexp-match-attribute attribute (regexp) #:transparent)
(define (make-regexp-match-attribute regexp)
  (regexp-match-attribute (format "string matching regexp ~v" regexp) regexp))

(struct string-contains-attribute attribute (value) #:transparent)
(define (make-string-contains-attribute str)
  (string-contains-attribute (format "string containing ~v" str) str))

(define (expect-string-contains? str)
  (define (contains-fault v)
    (and (not (string-contains? v str))
         (fault #:summary "a contained substring"
                #:expected (make-string-contains-attribute str)
                #:actual (make-self-attribute v))))
  (define exp
    (expect-and (expect-pred string?) (expect/singular contains-fault)))
  (expectation-rename exp 'string-contains?))
