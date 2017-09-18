#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [->expectation (-> any/c expectation?)]))

(module+ for-conversion
  (provide define-conversions
           <convert>))

(require (for-syntax racket/base
                     racket/syntax)
         racket/list
         racket/set
         syntax/parse/define
         expect/private/lite
         "data-set.rkt"
         (submod "data-dict.rkt" for-conversion)
         (submod "data-sequence.rkt" for-conversion)
         (submod "data-syntax.rkt" for-conversion))


(define (->expectation v)
  (cond
    [(expectation? v) v]
    [(list? v) (apply expect-list (map ->expectation v))]
    [(vector? v)
     (apply expect-vector (map ->expectation (vector->list v)))]
    [(set? v) (apply expect-set (set->list v))]
    [(hash? v)
     (define converted (map ->expectation (hash-values v)))
     (apply expect-hash (append-map list (hash-keys v) converted))]
    [(syntax? v)
     (if (syntax->list v)
         (expect-syntax-list (->expectation (syntax->list v)))
         (expect-syntax (->expectation (syntax-e v))))]
    [(boolean? v) (if v expect-true expect-false)]
    [else (expect-compare equal? v)]))

(define-syntax <convert> #f)

(begin-for-syntax
  (define-syntax-class arg-id
    #:literals (<convert>)
    (pattern <convert>
             #:attr id (generate-temporary)
             #:attr expr #'(->expectation id))
    (pattern id:id #:attr expr #'id)))

(define-syntax-parser define/expectation-conversion
  [(_ (id:id arg:arg-id ... . rest-arg:arg-id) contract-expr:expr)
   #'(begin
       (define (converted arg.id ... . rest-arg.id)
         (apply id arg.expr ... (map ->expectation rest-arg.id)))
       (provide (contract-out (rename converted id contract-expr))))]
  [(_ (id:id arg:arg-id ...) contract-expr:expr)
   #'(begin
       (define (converted arg.id ...) (id arg.expr ...))
       (provide (contract-out (rename converted id contract-expr))))])

(define-simple-macro (define-conversions [header contract] ...)
  (begin (define/expectation-conversion header contract) ...))
