#lang racket/base

(require expect/private/lite
         expect/private/util
         fancy-app
         racket/contract/base
         racket/dict
         racket/set
         syntax/parse/define
         "context.rkt"
         "data-set.rkt"
         (submod "data-sequence.rkt" for-internal))


(define (expect-dict-ref k value-exp)
  (expect/context (expect/proc value-exp (hash-ref _ k))
                  (make-dict-context k)))

(define (expect-dict-keys set-exp)
  (expect/context (expect/proc set-exp dict-keys-set) the-keys-context))

(define (expect-dict k+exps)
  (define key-exp-hash (apply hash k+exps))
  (define keys (slice k+exps #:step 2))
  (define keys-set (list->set keys))
  (define (present-keys d) (set-intersect (list->set (dict-keys d)) keys-set))
  (define (expect-key k) (expect-dict-ref k (hash-ref key-exp-hash k)))
  (define (present-keys-exp d)
    (apply expect-all (map expect-key (set->list (present-keys d)))))
  (expect-all (expect-dict-keys (apply expect-set keys))
              (expect/dependent present-keys-exp)))

(define (dict-keys-set d) (list->set (dict-keys d)))

(define-simple-macro
  (define-dict-expectations pred:id
    #:length len-id:id len-name:id
    #:keys keys-id:id keys-name:id
    #:ref ref-id:id ref-name:id
    #:all all-id:id all-name:id)
  (begin
    (define (exp/pred+name e n)
      (expectation-rename (expect-and (expect-pred pred) e) n))
    (define (len-id e) (exp/pred+name (expect-sequence-length e) 'len-name))
    (define (keys-id e) (exp/pred+name (expect-dict-keys e) 'keys-name))
    (define (ref-id k e) (exp/pred+name (expect-dict-ref k e) 'ref-name))
    (define (all-id . k+es) (exp/pred+name (expect-dict k+es) 'all-name))
    (provide
     (contract-out
      [len-id (-> expectation? expectation?)]
      [keys-id (-> expectation? expectation?)]
      [ref-id (-> any/c expectation? expectation?)]
      [all-id (rest->* (list any/c expectation?) expectation?)]))))

(module+ for-conversion
  (define-dict-expectations hash?
    #:length expect-hash-count hash-count
    #:keys expect-hash-keys hash-keys
    #:ref expect-hash-ref hash-ref
    #:all expect-hash hash))