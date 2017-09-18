#lang racket/base

(module+ for-internal
  (provide expect-sequence-length))

(module+ for-conversion
  (provide define-sequence-expectations))

(require expect/private/lite
         fancy-app
         racket/sequence
         syntax/parse/define
         "context.rkt")


(define (expect-sequence-length e)
  (expect/context (expect/proc e sequence-length) the-length-context))

(define (expect-sequence-ref e i)
  (expect/context (expect/proc e (sequence-ref _ i))
                  (make-sequence-context i)))

(define (expect-sequence-items exps)
  (define (->items-exp vs)
    (apply expect-all
           (for/list ([(_ i) (in-indexed vs)] [e exps])
             (expect-sequence-ref e i))))
  (expect/dependent ->items-exp))

(define (expect-sequence exps)
  (expect-all (expect-sequence-length (expect-eqv? (length exps)))
              (expect-sequence-items exps)))

(define-simple-macro
  (define-sequence-expectations pred:id
    #:length len-id:id len-name:id
    #:ref ref-id:id ref-name:id
    #:all all-id:id all-name:id)
  (begin
    (define (exp/pred+name e n)
      (expectation-rename (expect-and (expect-pred pred) e) n))
    (define (len-id e) (exp/pred+name (expect-sequence-length e) 'len-name))
    (define (ref-id e i) (exp/pred+name (expect-sequence-ref e i) 'ref-name))
    (define (all-id . es) (exp/pred+name (expect-sequence es) 'all-name))))
