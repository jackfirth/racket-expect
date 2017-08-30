#lang racket/base

(require racket/contract)

(module+ for-conversion
  (provide
   (contract-out
    [expect-hash (rest->* (list any/c expectation?) expectation?)]
    [expect-hash-count (-> expectation? expectation?)]
    [expect-hash-keys (-> expectation? expectation?)]
    [expect-hash-ref (-> any/c expectation? expectation?)])))

(require fancy-app
         racket/set
         expect/private/base
         expect/private/combinator
         expect/private/logic
         expect/private/util
         "data-collect.rkt"
         "data-set.rkt"
         (submod "compare.rkt" for-conversion))


(define expect-hash-count (expect/count _ hash-count))

(struct key-context context (value)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-key-context)

(define (key-context k)
  (make-key-context (format "value for key ~v" k) k))

(define (expect-hash-ref k value-exp)
  (expect/context (expect/proc value-exp (hash-ref _ k)) (key-context k)))

(struct key-set-context context ()
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-key-set-context)

(define key-set-context (make-key-set-context "the set of hash keys"))

(define (expect-hash-keys set-exp)
  (expect/context (expect/proc set-exp (λ (h) (list->set (hash-keys h))))
                  key-set-context))

(define (expect-hash . k+exps)
  (define keys (slice k+exps #:step 2))
  (define key-exp-hash (apply hash k+exps))
  (define (present-keys-exp h)
    (define present-keys
      (set-intersect (list->set (hash-keys h)) (list->set keys)))
    (apply expect-all
           (for/list ([k (in-set present-keys)])
             (expect-hash-ref k (hash-ref key-exp-hash k)))))
  (expect-and (expect-pred hash?)
              (expect-all (expect-hash-keys (apply expect-set keys))
                          (expect/dependent present-keys-exp))))
