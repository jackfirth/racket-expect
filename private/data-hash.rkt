#lang racket/base

(require racket/contract)

(module+ no-conversion
  (provide
   (contract-out
    [expect-hash (rest->* (list any/c expectation?) expectation?)]
    [expect-hash-count (-> expectation? expectation?)]
    [expect-hash-keys (-> expectation? expectation?)]
    [expect-hash-ref (-> any/c expectation? expectation?)])))

(require fancy-app
         racket/set
         "base.rkt"
         "combinator.rkt"
         "data-collect.rkt"
         "data-set.rkt"
         "logic.rkt"
         "util.rkt"
         (submod "data-set.rkt" no-conversion))

(module+ test
  (require racket/function
           rackunit))


(define expect-hash-count (expect/count _ hash-count))

(module+ test
  (require (submod "compare.rkt" no-conversion))
  (check-not-exn
   (thunk (expect! (hash 'a 1 'b 2) (expect-hash-count (expect-equal? 2))))))


(struct key-context context (value)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-key-context)

(define (key-context k)
  (make-key-context (format "value for key ~v" k) k))

(define (expect-hash-ref k value-exp)
  (expect/context (expect/proc value-exp (hash-ref _ k)) (key-context k)))

(module+ test
  (check-exn #rx"in: value for key 'foo"
             (thunk (expect! (hash 'foo 1)
                             (expect-hash-ref 'foo (expect-equal? 2))))))

(struct key-set-context context ()
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-key-set-context)

(define key-set-context (make-key-set-context "the set of hash keys"))

(define (expect-hash-keys set-exp)
  (expect/context (expect/proc set-exp (λ (h) (list->set (hash-keys h))))
                  key-set-context))

(module+ test
  (check-not-exn
   (thunk (expect! (hash 'foo 1) (expect-hash-keys (expect-set-member? 'foo)))))
  (check-exn #rx"in: the set of hash keys"
             (thunk (expect! (hash 'foo 1)
                             (expect-hash-keys (expect-set-member? 'bar))))))

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

(module+ test
  (define expect-foo1-bar2!
    (expect! _ (expect-hash 'foo (expect-equal? 1) 'bar (expect-equal? 2))))
  (check-not-exn  (thunk (expect-foo1-bar2! (hash 'foo 1 'bar 2))))
  (check-exn exn:fail:expect?
             (thunk (expect-foo1-bar2! (hash 'foo 1 'bar 2 'baz 3))))
  (check-exn exn:fail:expect?
             (thunk (expect-foo1-bar2! (hash 'foo 1))))
  (check-exn exn:fail:expect?
             (thunk (expect-foo1-bar2! (hash 'foo 1 'bar 5)))))
