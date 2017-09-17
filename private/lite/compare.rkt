#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [expect-compare (-> compare/c any/c expectation?)]
  [expect-not-compare (-> compare/c any/c expectation?)]
  [expect-contains? (-> contains/c any/c expectation?)]
  [expect-not-contains? (-> contains/c any/c expectation?)]
  [expect-contains-all? (-> contains/c list? expectation?)]
  [expect-contains-none? (-> contains/c list? expectation?)]
  [expect-eq? (-> any/c expectation?)]
  [expect-not-eq? (-> any/c expectation?)]
  [expect-eqv? (-> any/c expectation?)]
  [expect-not-eqv? (-> any/c expectation?)]
  [expect-not-equal? (-> any/c expectation?)]
  [expect-= (-> real? real? expectation?)]
  [struct (compare-attribute attribute)
    ([description string?] [proc compare/c] [other any/c])
    #:omit-constructor]
  [make-compare-attribute (-> compare/c any/c compare-attribute?)]
  [struct (contains-attribute attribute)
    ([description string?] [proc contains/c] [value any/c])
    #:omit-constructor]
  [make-contains-attribute (-> contains/c any/c contains-attribute?)]
  [make-contains-all-attribute (-> contains/c list? and-attribute?)]
  [make-contains-none-attribute (-> contains/c list? not-attribute?)]
  [struct (=-attribute attribute)
    ([description string?] [value real?] [epsilon real?]) #:omit-constructor]
  [make-=-attribute (-> real? real? =-attribute?)]))

(module+ for-sugar
  (provide define-attr-sugar))

(require fancy-app
         racket/format
         racket/list
         racket/string
         syntax/parse/define
         "base.rkt"
         "combinator.rkt"
         "logic.rkt")

(module+ test
  (require rackunit))


(define compare/c (-> any/c any/c any/c))
(define contains/c compare/c)

(struct compare-attribute attribute (proc other) #:transparent)
(define (make-compare-attribute proc other)
  (define desc (format "~a to ~v" (object-name proc) other))
  (compare-attribute desc proc other))

(define (expect-compare compare other)
  (define (make-fault v)
    (and (not (compare v other))
         (fault #:summary "a different value"
                #:expected (make-compare-attribute compare other)
                #:actual (make-self-attribute v))))
  (expectation-rename (expect/singular make-fault) (object-name compare)))

(define (expect-not-compare compare other)
  (define attr (make-not-attribute (make-compare-attribute compare other)))
  (define (make-fault v)
    (and (compare v other)
         (fault #:summary "a different value"
                #:expected attr
                #:actual (make-self-attribute v))))
  (define name (string->symbol (format "not-~a" (object-name compare))))
  (expectation-rename (expect/singular make-fault) name))

(define (expect-eq? v) (expect-compare eq? v))
(define (expect-eqv? v) (expect-compare eqv? v))
(define (expect-not-eq? v) (expect-not-compare eq? v))
(define (expect-not-eqv? v) (expect-not-compare eqv? v))
(define (expect-not-equal? v) (expect-not-compare equal? v))

(struct contains-attribute attribute (proc value) #:transparent)
(define (make-contains-attribute proc value)
  (define desc (format "~v contained with ~a" value (object-name proc)))
  (contains-attribute desc proc value))

(module+ test
  (define desc "'foo contained with hash-has-key?")
  (check-equal? (make-contains-attribute hash-has-key? 'foo)
                (contains-attribute desc hash-has-key? 'foo)))

(define (make-not-contains-attribute contains? v)
  (make-not-attribute (make-contains-attribute contains? v)))

(define (expect-contains? contains? v)
  (define (make-fault sub)
    (and (not (contains? sub v))
         (fault #:summary "a value to be contained"
                #:expected (make-contains-attribute contains? v)
                #:actual (make-self-attribute sub))))
  (expectation-rename (expect/singular make-fault) (object-name contains?)))

(define (expect-not-contains? contains? v)
  (define (make-fault sub)
    (and (contains? sub v)
         (fault #:summary "a value to not be contained"
                #:expected (make-not-contains-attribute contains? v)
                #:actual (make-self-attribute sub))))
  (define name (string->symbol (format "not-~a" (object-name contains?))))
  (expectation-rename (expect/singular make-fault) name))

(define (make-contains-all-attribute proc vs)
  (define str (string-join (map ~v vs) ", " #:before-last " and "))
  (define desc (format "~a contained with ~a" str (object-name proc)))
  (make-and-attribute (map (make-contains-attribute proc _) vs)
                      #:description desc))

(define (expect-contains-all? contains? vs*)
  (define (make-fault sub)
    (define vs (filter-not (contains? sub _) vs*))
    (and (not (empty? vs))
         (fault #:summary "values to be contained"
                #:expected (make-contains-all-attribute contains? vs)
                #:actual (make-self-attribute sub))))
  (expectation-rename (expect/singular make-fault) (object-name contains?)))

(define (make-contains-none-attribute proc vs)
  (define str (string-join (map ~v vs) ", " #:before-last " or "))
  (define desc (format "~a contained with ~a" str (object-name proc)))
  (make-not-attribute
   (make-or-attribute (map (make-contains-attribute proc _) vs)
                      #:description desc)))

(define (expect-contains-none? contains? vs*)
  (define (make-fault sub)
    (define vs (filter (contains? sub _) vs*))
    (and (not (empty? vs))
         (fault #:summary "values to not be contained"
                #:expected (make-contains-none-attribute contains? vs)
                #:actual (make-self-attribute sub))))
  (define name (string->symbol (format "not-~a" (object-name contains?))))
  (expectation-rename (expect/singular make-fault) name))

(define-simple-macro (define-attr-sugar [id:id pred:id proc:id] ...+)
  (begin
    (begin
      (define (id v) (make-compare-attribute proc v))
      (define (pred v)
        (and (compare-attribute? v) (equal? (compare-attribute-proc v) proc)))
      (provide
       (contract-out [id (-> any/c compare-attribute?)] [pred predicate/c])))
    ...))

(struct =-attribute attribute (value epsilon)
  #:transparent)

(define (make-=-attribute value tolerance)
  (define desc (format "= to ~v (with tolerance of ~v)" value tolerance))
  (=-attribute desc value tolerance))

(define (expect-= e tolerance)
  (define lower (- e tolerance))
  (define upper (+ e tolerance))
  (define (make-fault v)
    (and (not (<= lower v upper))
         (fault #:summary "a different number"
                #:expected (make-=-attribute e tolerance)
                #:actual (make-self-attribute v))))
  (expectation-rename (expect/singular make-fault) '=))
