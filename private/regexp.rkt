#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-regexp-match
   (->* (regexp?)
        ((or/c (listof (or/c string? bytes? #f expectation?))
               #f
               expectation?))
        expectation?)]
  [struct (regexp-match-context context)
    ([description string?] [regexp regexp?])]))

(require fancy-app
         "base.rkt"
         "combinator.rkt"
         "data/main.rkt"
         "logic.rkt")


(define (expect-regexp-match pattern [result-exp expect-not-false])
  (expect-and (expect-disjoin string? bytes? path? input-port?)
              (expect/context (expect/proc (->expectation result-exp)
                                           (regexp-match pattern _))
                              (make-regexp-match-context pattern))))

(struct regexp-match-context context (regexp) #:transparent)
(define (make-regexp-match-context regexp)
  (regexp-match-context (format "the results of matching ~v" regexp)
                        regexp))

(module+ main
  (require "fail.rkt")
  (expect! "12x4x6" (expect-regexp-match #rx"x." '("x4"))))
