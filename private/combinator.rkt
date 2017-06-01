#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect/context (-> expectation? context? expectation?)]
  [expect/proc (-> expectation? (-> any/c any/c) expectation?)]
  [expect-all (rest-> expectation? expectation?)]
  [expect-and (rest-> expectation? expectation?)]
  [expect-list (rest-> expectation? expectation?)]
  [expect-vector (rest-> expectation? expectation?)]
  [expect-not-raise expectation?]))

(require fancy-app
         racket/format
         racket/function
         racket/list
         racket/stream
         "base.rkt"
         "data.rkt"
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

(struct arity-includes-attribute attribute (num-positional kws-okay?)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-arity-includes-attribute)

(define (arity-includes-attribute num-positional
                                  #:keywords-okay? [kws-okay? #f])
  (make-arity-includes-attribute
   (format "arity including ~a positional argument~a and ~a keyword arguments"
           num-positional
           (if (= num-positional 1) "" "s")
           (if kws-okay? "any" "no"))
   num-positional
   kws-okay?))

(module+ test
  (define plural-desc
    "arity including 5 positional arguments and no keyword arguments")
  (check-equal? (attribute-description (arity-includes-attribute 5))
                plural-desc)
  (define singular-kw-desc
    "arity including 1 positional argument and any keyword arguments")
  (define singular-kw-attr (arity-includes-attribute 1 #:keywords-okay? #t))
  (check-equal? (attribute-description singular-kw-attr) singular-kw-desc))

(struct arity-attribute attribute (value)
  #:transparent #:omit-define-syntaxes #:constructor-name make-arity-attribute)

(define (arity-attribute proc)
  (define arity (procedure-arity proc))
  (make-arity-attribute (format "arity of ~a" arity) arity))

(define (expect-procedure-arity-includes? k #:keywords-okay? [kws-okay? #f])
  (define attr (arity-includes-attribute k #:keywords-okay? kws-okay?))
  (expectation
   (λ (proc)
     (if (procedure-arity-includes? proc k kws-okay?)
         (list)
         (list (fault #:summary "a procedure accepting no arguments"
                      #:expected attr
                      #:actual (arity-attribute proc)))))))

(struct not-raise-attribute attribute () #:transparent)
(define the-not-raise-attribute (not-raise-attribute "no value raised"))

(struct raise-attribute attribute (value)
  #:transparent #:omit-define-syntaxes #:constructor-name make-raise-attribute)

(define (raise-attribute raised)
  (make-raise-attribute (format "raised ~v" raised)
                        raised))

(define (raise-fault raised)
  (fault #:summary "no value raised during procedure call"
         #:expected the-not-raise-attribute
         #:actual (raise-attribute raised)))

(define expect-not-raise
  (expect-and (expect-pred procedure?)
              (expect-procedure-arity-includes? 0)
              (expectation
               (λ (proc)
                 (with-handlers ([(const #t) (λ (v) (list (raise-fault v)))])
                   (proc)
                   (list))))))

(module+ test
  (check-equal? (expectation-apply/faults expect-not-raise void) (list))
  (check-equal? (expectation-apply/faults expect-not-raise identity)
                (list (fault #:summary "a procedure accepting no arguments"
                             #:expected (arity-includes-attribute 0)
                             #:actual (arity-attribute identity))))
  (check-equal? (expectation-apply/faults expect-not-raise (thunk (raise 'foo)))
                (list (fault #:summary "no value raised during procedure call"
                             #:expected the-not-raise-attribute
                             #:actual (raise-attribute 'foo)))))
