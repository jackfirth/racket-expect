#lang racket/base

(require racket/contract)

(provide
 define-conversions
 <convert>
 (contract-out
  [expectation-convert (-> expectation-convertible? expectation?)]
  [expectation-convertible? predicate/c]))

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/function
         racket/list
         racket/set
         syntax/parse/define
         "base.rkt"
         "compare.rkt"
         "data-hash.rkt"
         "data-list.rkt"
         "data-set.rkt"
         "data-vector.rkt"
         "logic.rkt")

(module+ test
  (require rackunit))


(define expectation-convertible?
  (or/c expectation?
        list? vector? set? hash?
        boolean? number? string? symbol? char?))

(define (expectation-convert v)
  (cond
    [(expectation? v) v]
    [(list? v) (apply expect-list (map expectation-convert v))]
    [(vector? v)
     (apply expect-vector (map expectation-convert (vector->list v)))]
    [(set? v) (apply expect-set (set->list v))]
    [(hash? v)
     (define converted (map expectation-convert (hash-values v)))
     (apply expect-hash (append-map list (hash-keys v) converted))]
    [(boolean? v) (if v expect-true expect-false)]
    [((disjoin number? string? symbol? char?) v) (expect-equal? v)]))

(module+ test
  (check-pred expectation? (expectation-convert 'foo))
  (check-pred expectation? (expectation-convert #t))
  (check-pred expectation? (expectation-convert expect-true))
  (check-pred expectation? (expectation-convert (list 1 2 3)))
  (check-pred expectation? (expectation-convert (vector 1 2 3)))
  (check-pred expectation? (expectation-convert (hash 'a 1)))
  (check-pred expectation? (expectation-convert (set 1 2 3)))
  (check-pred expectation?
              (expectation-convert
               (hash 'a (list 1 2 3)
                     'b (vector "foo" "bar")
                     'c (set 'a 'b 'c)
                     'd #f))))

(define-syntax <convert> #f)

(begin-for-syntax
  (define-syntax-class arg-id
    #:literals (<convert>)
    (pattern <convert>
             #:attr id (generate-temporary)
             #:attr expr #'(expectation-convert id))
    (pattern id:id #:attr expr #'id)))

(define-syntax-parser define/expectation-conversion
  [(_ (id:id arg:arg-id ... . rest-arg:arg-id) contract-expr:expr)
   #'(begin
       (define (converted arg.id ... . rest-arg.id)
         (apply id arg.expr ... (map expectation-convert rest-arg.id)))
       (provide (contract-out (rename converted id contract-expr))))]
  [(_ (id:id arg:arg-id ...) contract-expr:expr)
   #'(begin
       (define (converted arg.id ...) (id arg.expr ...))
       (provide (contract-out (rename converted id contract-expr))))])

(define-simple-macro (define-conversions [header contract] ...)
  (begin (define/expectation-conversion header contract) ...))
