#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-syntax (-> any/c expectation?)]
  [expect-expand (->* (expectation?) (#:namespace namespace?) expectation?)]
  [expect-expand-once (->* (expectation?)
                           (#:namespace namespace?)
                           expectation?)]
  [expect-syntax-exn (->* ()
                          ((or/c string? regexp? expectation?)
                           #:namespace namespace?)
                          expectation?)]
  [struct (datum-context context) ([description string?]) #:omit-constructor]
  [the-datum-context datum-context?]))

(require arguments
         racket/function
         "base.rkt"
         "combinator.rkt"
         "data/convert-base.rkt"
         "function.rkt"
         "logic.rkt"
         "regexp.rkt"
         "struct.rkt")


(struct datum-context context () #:transparent)
(define the-datum-context (datum-context "the syntax's datum"))

(define (expect-syntax exp)
  (define anon-exp
    (expect-and (expect-pred syntax?)
                (expect/context (expect/proc (->expectation exp) syntax->datum)
                                the-datum-context)))
  (expectation-rename anon-exp 'syntax))

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
