#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect/context (-> expectation? context? expectation?)]
  [expect-map (-> expectation? (-> any/c any/c) expectation?)]
  [expect-all (->* () #:rest (listof expectation?) expectation?)]
  [expect-and (->* () #:rest (listof expectation?) expectation?)]
  [expect-if (-> expectation? predicate/c expectation?)]))

(require fancy-app
         racket/list
         racket/stream
         "base.rkt")


(define (expect/context exp ctxt)
  (expectation
   (λ (v)
     (define (add-context flt)
       (define new-ctxts (cons ctxt (fault-contexts flt)))
       (fault #:summary (fault-summary flt)
              #:expected (fault-expected flt)
              #:actual (fault-actual flt)
              #:contexts new-ctxts))
     (map add-context (expectation-apply/faults exp v)))))

(define (expect-map exp f)
  (expectation (λ (v) (expectation-apply/faults exp (f v)))))

(define (expect-all . exps)
  (expectation (λ (v) (append-map (expectation-apply/faults _ v) exps))))

(define (expect-and . exps)
  (expectation
   (λ (v)
     (define faults-stream (stream-map (expectation-apply/faults _ v) exps))
     (for/first ([faults (in-stream faults-stream)]
                 #:unless (empty? faults))
       faults))))

(define (expect-if exp pred)
  (expectation (λ (v) (if (pred v) (expectation-apply/faults exp v) (list)))))
