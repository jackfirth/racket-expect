#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect/context (-> expectation? context? expectation?)]
  [expect/dependent (-> (-> any/c expectation?) expectation?)]
  [expect/proc (-> expectation? (-> any/c any/c) expectation?)]
  [expect/singular (-> (-> any/c (or/c fault? #f)) expectation?)]))

(require arguments
         fancy-app
         racket/format
         racket/function
         racket/list
         racket/stream
         "base.rkt"
         "util.rkt")

(module+ test
  (require rackunit))


(define (expect/context exp ctxt)
  (define (add-context flt)
    (define new-ctxts (cons ctxt (fault-contexts flt)))
    (fault #:summary (fault-summary flt)
           #:expected (fault-expected flt)
           #:actual (fault-actual flt)
           #:contexts new-ctxts))
  (expectation (λ (v) (map add-context (expectation-apply exp v)))))

(define (expect/proc exp f)
  (expectation (λ (v) (expectation-apply exp (f v)))))

(define (expect/dependent exp-func)
  (expectation (λ (v) (expectation-apply (exp-func v) v))))

(define (expect/singular maybe-fault-func)
  (expectation
   (λ (v) (define flt (maybe-fault-func v)) (if flt (list flt) (list)))))
