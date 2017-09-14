#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-call (-> arguments? expectation? expectation?)]
  [expect-apply (-> procedure? expectation? expectation?)]
  [expect-not-raise expectation?]
  [expect-raise (-> any/c expectation?)]
  [expect-return (rest-> any/c expectation?)]
  [expect-return* (-> (or/c list? expectation?) expectation?)]
  [struct (return-context context)
    ([description string?]) #:omit-constructor]
  [the-return-context return-context?]
  [struct (raise-context context)
    ([description string?]) #:omit-constructor]
  [the-raise-context raise-context?]
  [struct (call-context context)
    ([description string?] [args arguments?]) #:omit-constructor]
  [make-call-context (-> arguments? call-context?)]
  [struct (apply-context context)
    ([description string?] [proc procedure?]) #:omit-constructor]
  [make-apply-context (-> procedure? apply-context?)]
  [struct (arity-context context) ([description string?]) #:omit-constructor]
  [the-arity-context arity-context?]
  [struct (arity-includes-attribute attribute)
    ([description string?] [value procedure-arity?])
    #:omit-constructor]
  [make-arity-includes-attribute (-> procedure-arity?
                                     arity-includes-attribute?)]))

(require arguments
         fancy-app
         racket/format
         racket/function
         racket/match
         racket/string
         "base.rkt"
         "combinator.rkt"
         "data/main.rkt"
         "logic.rkt"
         "util.rkt")

(module+ test
  (require rackunit))


(define (plural-s n) (if (equal? n 1) "" "s"))

(struct arity-includes-attribute attribute (value) #:transparent)
(define (make-arity-includes-attribute arity)
  (define msg
    (match (normalize-arity arity)
      [(list) "the impossible arity (empty case-lambda)"]
      [(? exact-nonnegative-integer? n)
       (format "arity accepting ~a argument~a" n (plural-s n))]
      [(arity-at-least n)
       (format "arity accepting at least ~a argument~a" n (plural-s n))]
      [(list (? exact-nonnegative-integer? n) ...)
       (format "arity accepting ~a arguments"
               (string-join (map ~a n) ", " #:before-last ", or "))]
      [(list (? exact-nonnegative-integer? n) ... (arity-at-least m))
       (format "arity accepting ~a, or at least ~a arguments"
               (string-join (map ~a n) ", ") m)]))
  (arity-includes-attribute msg arity))

(module+ test
  (define (arity-attr-description v)
    (attribute-description (make-arity-includes-attribute v)))
  (check-equal? (arity-attr-description 1) "arity accepting 1 argument")
  (check-equal? (arity-attr-description 5) "arity accepting 5 arguments")
  (check-equal? (arity-attr-description 0) "arity accepting 0 arguments")
  (check-equal? (arity-attr-description (list))
                "the impossible arity (empty case-lambda)")
  (check-equal? (arity-attr-description (arity-at-least 1))
                "arity accepting at least 1 argument")
  (check-equal? (arity-attr-description (arity-at-least 3))
                "arity accepting at least 3 arguments")
  (check-equal? (arity-attr-description (arity-at-least 0))
                "arity accepting at least 0 arguments")
  (check-equal? (arity-attr-description (list 1 3 5))
                "arity accepting 1, 3, or 5 arguments")
  (check-equal? (arity-attr-description (list 1 3 5 (arity-at-least 7)))
                "arity accepting 1, 3, 5, or at least 7 arguments"))

(struct arity-context context () #:transparent)
(define the-arity-context (arity-context "the procedure's arity"))

(struct raise-context context () #:transparent)
(define the-raise-context (raise-context "the raised value"))

(struct return-context context () #:transparent)
(define the-return-context (return-context "the return values list"))

(struct call-context context (args) #:transparent)
(define (make-call-context args)
  (call-context (format "call with ~v" args) args))

(struct apply-context context (proc) #:transparent)
(define (make-apply-context proc)
  (define n (object-name proc))
  (apply-context (format "application to ~a"
                         (if n (format "procedure ~a" n) (~v proc)))
                 proc))

(define (expect-proc-arity arity-exp)
  (expect/context (expect/proc arity-exp procedure-arity) the-arity-context))

(define (expect-arity-includes? arity)
  (define (make-fault actual-arity)
    (and (not (arity-includes? actual-arity arity))
         (fault #:summary "a more inclusive arity"
                #:expected (make-arity-includes-attribute arity)
                #:actual (make-self-attribute actual-arity))))
  (expect-and (expect-pred procedure-arity?) (expect/singular make-fault)))

(define (raise-fault raised)
  (fault #:summary "no value raised"
         #:expected the-none-attribute
         #:actual (make-self-attribute raised)
         #:contexts (list the-raise-context)))

(define (expect-thunk exp)
  (expect-and (expect-pred procedure?)
              (expect-proc-arity (expect-arity-includes? 0))
              exp))

(define (make-not-raise-fault proc)
  (with-handlers ([(const #t) raise-fault]) (proc) #f))

(define expect-not-raise (expect-thunk (expect/singular make-not-raise-fault)))

(define exp-raise-any-fault
  (fault #:summary "any value raised"
         #:expected the-any-attribute
         #:actual the-none-attribute
         #:contexts (list the-raise-context)))

(define (expect-raise v)
  (define exp/context (expect/context (->expectation v) the-raise-context))
  (expect-thunk
   (expectation
    (λ (proc)
      (with-handlers ([(const #t) (expectation-apply exp/context _)])
        (proc)
        (list exp-raise-any-fault))))))

(define (expect-return . vs) (expect-return* (apply expect-list vs)))

(define (expect-return* v)
  (define exp/context (expect/context (->expectation v) the-return-context))
  (expect-thunk
   (expectation
    (λ (proc)
      (with-handlers ([(const #t) (λ (e) (list (raise-fault e)))])
        (define results (call-with-values proc list))
        (expectation-apply exp/context results))))))

(define (expect-call args call-exp)
  (define call-exp* (expect/context call-exp (make-call-context args)))
  (define num-pos (length (arguments-positional args)))
  (expect-and (expect-pred procedure?)
              (expect-proc-arity (expect-arity-includes? num-pos))
              (expectation
               (λ (proc)
                 (define (call) (apply/arguments proc args))
                 (expectation-apply call-exp* call)))))

(define (expect-apply proc call-exp)
  (define call-exp* (expect/context call-exp (make-apply-context proc)))
  (expect-and (expect-pred arguments?)
              (expectation
               (λ (args)
                 (define (call) (apply/arguments proc args))
                 (expectation-apply call-exp* call)))))
