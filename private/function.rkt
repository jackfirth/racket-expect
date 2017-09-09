#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-call (-> arguments? expectation? expectation?)]
  [expect-not-raise expectation?]
  [expect-raise (-> any/c expectation?)]
  [expect-return (rest-> any/c expectation?)]
  [expect-return* (-> any/c expectation?)]
  [struct (arity-includes-attribute attribute)
    ([description string?]
     [num-positional exact-nonnegative-integer?]
     [kws-okay? boolean?])
    #:omit-constructor]
  [struct (arity-attribute attribute)
    ([description string?] [value procedure-arity?]) #:omit-constructor]
  [struct (not-raise-attribute attribute)
    ([description string?]) #:omit-constructor]
  [struct (raise-attribute attribute)
    ([description string?] [value any/c]) #:omit-constructor]
  [struct (raise-any-attribute attribute)
    ([description string?]) #:omit-constructor]
  [struct (raise-context context)
    ([description string?]) #:omit-constructor]
  [struct (return-context context)
    ([description string?]) #:omit-constructor]
  [struct (call-context context)
    ([description string?] [args arguments?]) #:omit-constructor]))

(require arguments
         fancy-app
         racket/function
         "base.rkt"
         "combinator.rkt"
         "data/main.rkt"
         "logic.rkt"
         "util.rkt")

(module+ test
  (require rackunit))


(struct arity-includes-attribute attribute (num-positional kws-okay?)
  #:transparent)
(define (make-arity-includes-attribute num-positional
                                       #:keywords-okay? [kws-okay? #f])
  (arity-includes-attribute
   (format "arity including ~a positional argument~a and ~a keyword arguments"
           num-positional
           (if (= num-positional 1) "" "s")
           (if kws-okay? "any" "no"))
   num-positional
   kws-okay?))

(struct arity-attribute attribute (value) #:transparent)
(define (make-arity-attribute proc)
  (define arity (procedure-arity proc))
  (arity-attribute (format "arity of ~a" arity) arity))

(struct not-raise-attribute attribute () #:transparent)
(define the-not-raise-attribute (not-raise-attribute "no value raised"))

(struct raise-attribute attribute (value) #:transparent)
(define (make-raise-attribute raised)
  (raise-attribute (format "raised ~v" raised) raised))

(struct raise-any-attribute attribute () #:transparent)
(define the-raise-any-attribute (raise-any-attribute "raised a value"))

(struct raise-context context () #:transparent)
(define the-raise-context (raise-context "the raised value"))

(struct return-context context () #:transparent)
(define the-return-context (return-context "the return values list"))

(struct call-context context (args) #:transparent)
(define (make-call-context args)
  (call-context (format "call with ~v" args) args))

(define (expect-procedure-arity-includes? args)
  (define k (length (arguments-positional args)))
  (define kws-okay? (not (hash-empty? (arguments-keyword args))))
  (define attr (make-arity-includes-attribute k #:keywords-okay? kws-okay?))
  (define (make-fault proc)
    (and (not (procedure-arity-includes? proc k kws-okay?))
         (fault #:summary "a procedure accepting no arguments"
                #:expected attr
                #:actual (make-arity-attribute proc))))
  (expect-and (expect-pred procedure?) (expect/singular make-fault)))

(define (raise-fault raised)
  (fault #:summary "no value raised during procedure call"
         #:expected the-not-raise-attribute
         #:actual (make-raise-attribute raised)))

(define (expect-thunk exp)
  (expect-and (expect-procedure-arity-includes? (arguments)) exp))

(define (make-not-raise-fault proc)
  (with-handlers ([(const #t) raise-fault]) (proc) #f))

(define expect-not-raise (expect-thunk (expect/singular make-not-raise-fault)))

(define exp-raise-any-fault
  (fault #:summary "a value raised during procedure call"
         #:expected the-raise-any-attribute
         #:actual the-not-raise-attribute))

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
  (expect-and (expect-procedure-arity-includes? args)
              (expectation
               (λ (proc)
                 (define (call) (apply/arguments proc args))
                 (define call-exp/context
                   (expect/context call-exp (make-call-context args)))
                 (expectation-apply call-exp/context call)))))
