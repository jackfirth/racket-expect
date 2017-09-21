#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [struct (dict-context context)
    ([description string?] [key any/c]) #:omit-constructor]
  [make-dict-context (-> any/c dict-context?)]
  [struct (sequence-context context)
    ([description string?] [position exact-nonnegative-integer?])
    #:omit-constructor]
  [make-sequence-context (-> exact-nonnegative-integer? sequence-context?)]
  [the-length-context splice-context?]
  [the-keys-context splice-context?]
  [syntax-context? predicate/c]
  [the-box-context context?]))

(module+ for-internal
  (provide the-syntax-e-context
           the-syntax-list-context))

(require expect/private/function-context
         expect/private/lite
         racket/dict
         racket/sequence
         racket/set)

(module+ test
  (require rackunit))


(struct dict-context context (key) #:transparent)
(struct sequence-context context (position) #:transparent)
(struct box-context context () #:transparent)

(define (make-dict-context key)
  (dict-context (format "value for key ~v" key) key))

(module+ test
  (check-equal? (make-dict-context 'foo)
                (dict-context "value for key 'foo" 'foo)))

(define (make-sequence-context position)
  (sequence-context (format "item at position ~v" position) position))

(module+ test
  (check-equal? (make-sequence-context 123)
                (sequence-context "item at position 123" 123)))

(define the-box-context (box-context "the box's value"))

;; This is so the data structure expectations can provide the same fault
;; contexts that an equivalent use of expect-apply would produce without
;; causing a cyclic dependency between expect/private/function and
;; expect/private/data.

(define (make-apply1-context f #:description [desc* #f])
  (define ctxts
    (list (make-apply-context f) the-return-context (make-sequence-context 1)))
  (define desc (or desc* (format "the return value of ~a" (object-name f))))
  (make-splice-context ctxts #:description desc))

(define the-length-context
  (make-apply1-context sequence-length #:description "the number of items"))

(define the-keys-context
  (make-splice-context (list (make-apply1-context dict-keys)
                             (make-apply1-context list->set))
                       #:description "the set of keys"))

(define the-syntax-e-context (make-apply1-context syntax-e))
(define the-syntax-list-context (make-apply1-context syntax->list))

(define (syntax-context? v)
  (or (equal? v the-syntax-e-context) (equal? v the-syntax-list-context)))
