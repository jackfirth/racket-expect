#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect/context (-> expectation? context? expectation?)]
  [expect/proc (-> expectation? (-> any/c any/c) expectation?)]
  [expect-all (rest-> expectation? expectation?)]
  [expect-and (rest-> expectation? expectation?)]
  [expect-list (rest-> expectation? expectation?)]
  [expect-vector (rest-> expectation? expectation?)]))

(require fancy-app
         racket/format
         racket/list
         racket/stream
         "base.rkt"
         "data.rkt"
         "util.rkt")


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


(struct item-context context (type index)
  #:transparent #:omit-define-syntaxes #:constructor-name make-item-context)

(define (item-context type index)
  (make-item-context (format "~a item ~a" type index) type index))

(define ((expect-item type ref) exp index)
  (expect/proc (expect/context exp (item-context type index))
               (ref _ index)))

(define expect-list-item (expect-item 'list list-ref))
(define expect-vector-item (expect-item 'vector vector-ref))

(struct count-attribute attribute (type count)
  #:transparent #:omit-define-syntaxes #:constructor-name make-count-attribute)

(define/contract (count-attribute type count)
  (-> (or/c 'vector 'list) exact-positive-integer? count-attribute?)
  (make-count-attribute (format "~a of size ~a" type count) type count))

(define/contract (countable-type vs)
  (-> (or/c vector? list?) (or/c 'vector 'list))
  (cond [(vector? vs) 'vector] [(list? vs) 'list]))

(define (expect-count expected-count count-proc items-desc)
  (define (~items v) (~a v items-desc #:separator " "))
  (expectation
   (λ (vs)
     (define type (countable-type vs))
     (define count (count-proc vs))
     (define (count-fault summary)
       (fault #:summary summary
              #:expected (count-attribute type expected-count)
              #:actual (count-attribute type count)))
     (cond [(< expected-count count) (list (count-fault (~items "fewer")))]
           [(< count expected-count) (list (count-fault (~items "more")))]
           [else (list)]))))

(define (expect-items-combined item-exps length-proc)
  (expectation
   (λ (vs)
     (define count (min (length-proc vs) (length item-exps)))
     (define combined (apply expect-all (take item-exps count)))
     (expectation-apply/faults combined vs))))

(define (expect-list . exps)
  (define item-exps
    (for/list ([exp (in-list exps)] [i (in-naturals)])
      (expect-list-item exp i)))
  (expect-and (expect-pred list?)
              (expect-all (expect-count (length exps) length "list items")
                          (expect-items-combined item-exps length))))

(define (expect-vector . exps)
  (define item-exps
    (for/list ([exp (in-list exps)] [i (in-naturals)])
      (expect-vector-item exp i)))
  (define num-exps (length exps))
  (expect-and (expect-pred vector?)
              (expect-all (expect-count num-exps vector-length "vector items")
                          (expect-items-combined item-exps vector-length))))
