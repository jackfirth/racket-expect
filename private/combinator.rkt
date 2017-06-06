#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect/context (-> expectation? context? expectation?)]
  [expect/derive (-> (-> any/c expectation?) expectation?)]
  [expect/proc (-> expectation? (-> any/c any/c) expectation?)]
  [expect/single-fault (-> (-> any/c (or/c fault? #f)) expectation?)]))

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
  (expectation
   (λ (v)
     (define (add-context flt)
       (define new-ctxts (cons ctxt (fault-contexts flt)))
       (fault #:summary (fault-summary flt)
              #:expected (fault-expected flt)
              #:actual (fault-actual flt)
              #:contexts new-ctxts))
     (map add-context (expectation-apply exp v)))))

(define (expect/proc exp f)
  (expectation (λ (v) (expectation-apply exp (f v)))))

(define (expect/derive exp-func)
  (expectation (λ (v) (expectation-apply (exp-func v) v))))

(define (expect/single-fault maybe-fault-func)
  (expectation
   (λ (v) (define flt (maybe-fault-func v)) (if flt (list flt) (list)))))
