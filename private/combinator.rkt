#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect/context (-> expectation? context? expectation?)]
  [expect/proc (-> expectation? (-> any/c any/c) expectation?)]
  [expect-all (->* () #:rest (listof expectation?) expectation?)]
  [expect-and (->* () #:rest (listof expectation?) expectation?)]
  [expect-list (->* () #:rest (listof expectation?) expectation?)]))

(require fancy-app
         racket/format
         racket/list
         racket/stream
         "base.rkt"
         "data.rkt")


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

(define (expect/proc exp f)
  (expectation (λ (v) (expectation-apply/faults exp (f v)))))

(define (expect-all . exps)
  (expectation (λ (v) (append-map (expectation-apply/faults _ v) exps))))

(define (expect-and . exps)
  (expectation
   (λ (v)
     (define faults-stream (stream-map (expectation-apply/faults _ v) exps))
     (or (for/first ([faults (in-stream faults-stream)]
                     #:unless (empty? faults))
           faults)
         (list)))))

;; Compound data constructors

(struct list-item-context context (index) #:transparent)

(define (expect-list-item exp index)
  (define ctxt
    (list-item-context (~a "list item" index #:separator " ") index))
  (expect/proc (expect/context exp ctxt) (list-ref _ index)))

(struct length-attribute attribute (length) #:transparent)

(define (make-length-attribute n)
  (length-attribute (format "length of ~v" n) n))

(define (expect-count expected-count count-proc items-desc)
  (define (~items v) (~a v items-desc #:separator " "))
  (expectation
   (λ (vs)
     (define count (count-proc vs))
     (define (count-fault summary)
       (fault #:summary summary
              #:expected (make-length-attribute expected-count)
              #:actual (make-length-attribute count)))
     (cond [(< expected-count count) (list (count-fault (~items "fewer")))]
           [(< count expected-count) (list (count-fault (~items "more")))]
           [else (list)]))))

(define (expect-list . exps)
  (define item-exps
    (for/list ([exp (in-list exps)] [i (in-naturals)])
      (expect-list-item exp i)))
  (define count-exp (expect-count (length exps) length "list items"))
  (define item-exps/count-guard
    (expectation
     (λ (vs)
       (define count (min (length vs) (length exps)))
       (define combined-item-exp (apply expect-all (take item-exps count)))
       (expectation-apply/faults combined-item-exp vs))))
  (expect-and (expect-pred list?) (expect-all count-exp item-exps/count-guard)))
