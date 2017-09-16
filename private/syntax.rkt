#lang racket/base

;; This module is meant for expectations on syntax operations like expansion and
;; evaluation. For expectations on the contents of the syntax objects
;; themselves, see expect/private/data/data-syntax.

(require racket/contract/base)

(provide
 (contract-out
  [expect-expand (->* (expectation?) (#:namespace namespace?) expectation?)]
  [expect-expand-once (->* (expectation?)
                           (#:namespace namespace?)
                           expectation?)]
  [expect-syntax-exn (->* ()
                          ((or/c string? regexp? expectation?)
                           #:namespace namespace?)
                          expectation?)]))

(require arguments
         racket/function
         "base.rkt"
         "combinator.rkt"
         "data.rkt"
         "function.rkt"
         "logic.rkt"
         "regexp.rkt"
         "struct.rkt")


(define (expect-expand* f exp ns)
  (define (around thnk) (parameterize ([current-namespace ns]) (thnk)))
  (define anon-exp
    (expect-and (expect-pred syntax?)
                (expect/proc (expect/around (expect-apply f exp) around)
                             arguments)))
  (expectation-rename anon-exp (object-name f)))

(define (expect-expand exp #:namespace [ns (current-namespace)])
  (expect-expand* expand exp ns))

(define (expect-expand-once exp #:namespace [ns (current-namespace)])
  (expect-expand* expand-once exp ns))

(define (expect-syntax-exn [msg-exp expect-any]
                           #:namespace [ns (current-namespace)])
  (define msg-exp*
    (if (regexp? msg-exp)
        (expect-regexp-match msg-exp)
        (->expectation msg-exp)))
  (define raise-exp
    (expect-raise (expect-struct exn:fail:syntax [exn-message msg-exp*])))
  (expectation-rename (expect-expand raise-exp #:namespace ns) 'syntax-exn))
