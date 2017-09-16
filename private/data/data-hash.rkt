#lang racket/base

(require racket/contract)

(module+ for-conversion
  (provide
   (contract-out
    [expect-hash (rest->* (list any/c expectation?) expectation?)]
    [expect-hash-count (-> expectation? expectation?)]
    [expect-hash-keys (-> expectation? expectation?)]
    [expect-hash-ref (-> any/c expectation? expectation?)])))

(require arguments
         fancy-app
         racket/set
         expect/private/base
         expect/private/combinator
         expect/private/logic
         expect/private/util
         "context.rkt"
         "data-set.rkt"
         "kernel-apply.rkt"
         (submod "data-list.rkt" for-conversion))


(define (expect-hash-count exp)
  (expectation-rename (expect-apply1 hash-count exp) 'hash-count))

(define (expect-hash-ref k value-exp)
  (define exp
    (expect/context (expect/proc value-exp (hash-ref _ k))
                    (make-dict-context k)))
  (expectation-rename exp 'hash-ref))

(define (expect-hash-keys set-exp)
  (define anon-exp
    (expect-apply1 hash-keys (expect-apply1 list->set set-exp)))
  (expectation-rename anon-exp 'hash-keys))

(define (expect-hash . k+exps)
  (define keys (slice k+exps #:step 2))
  (define key-exp-hash (apply hash k+exps))
  (define (present-keys-exp h)
    (define present-keys
      (set-intersect (list->set (hash-keys h)) (list->set keys)))
    (apply expect-all
           (for/list ([k (in-set present-keys)])
             (expect-hash-ref k (hash-ref key-exp-hash k)))))
  (define exp
    (expect-and (expect-pred hash?)
                (expect-all (expect-hash-keys (apply expect-set keys))
                            (expect/dependent present-keys-exp))))
  (expectation-rename exp 'hash))
