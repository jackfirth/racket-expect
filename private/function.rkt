#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-call (-> arguments? expectation? expectation?)]
  [expect-not-raise expectation?]))

(module+ no-conversion
  (provide
   (contract-out
    [expect-return (-> expectation? expectation?)]
    [expect-raise (-> expectation? expectation?)])))

(require arguments
         fancy-app
         racket/function
         "base.rkt"
         "combinator.rkt"
         "compare.rkt"
         "logic.rkt")

(module+ test
  (require rackunit
           (submod "compare.rkt" no-conversion)))


(struct arity-includes-attribute attribute (num-positional kws-okay?)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-arity-includes-attribute)

(define (arity-includes-attribute num-positional
                                  #:keywords-okay? [kws-okay? #f])
  (make-arity-includes-attribute
   (format "arity including ~a positional argument~a and ~a keyword arguments"
           num-positional
           (if (= num-positional 1) "" "s")
           (if kws-okay? "any" "no"))
   num-positional
   kws-okay?))

(module+ test
  (define plural-desc
    "arity including 5 positional arguments and no keyword arguments")
  (check-equal? (attribute-description (arity-includes-attribute 5))
                plural-desc)
  (define singular-kw-desc
    "arity including 1 positional argument and any keyword arguments")
  (define singular-kw-attr (arity-includes-attribute 1 #:keywords-okay? #t))
  (check-equal? (attribute-description singular-kw-attr) singular-kw-desc))

(struct arity-attribute attribute (value)
  #:transparent #:omit-define-syntaxes #:constructor-name make-arity-attribute)

(define (arity-attribute proc)
  (define arity (procedure-arity proc))
  (make-arity-attribute (format "arity of ~a" arity) arity))

(define (expect-procedure-arity-includes? k #:keywords-okay? [kws-okay? #f])
  (define attr (arity-includes-attribute k #:keywords-okay? kws-okay?))
  (define (make-fault proc)
    (and (not (procedure-arity-includes? proc k kws-okay?))
         (fault #:summary "a procedure accepting no arguments"
                #:expected attr
                #:actual (arity-attribute proc))))
  (expect/singular make-fault))

(struct not-raise-attribute attribute () #:transparent)
(define the-not-raise-attribute (not-raise-attribute "no value raised"))

(struct raise-attribute attribute (value)
  #:transparent #:omit-define-syntaxes #:constructor-name make-raise-attribute)

(define (raise-attribute raised)
  (make-raise-attribute (format "raised ~v" raised) raised))

(define (raise-fault raised)
  (fault #:summary "no value raised during procedure call"
         #:expected the-not-raise-attribute
         #:actual (raise-attribute raised)))

(define (expect-thunk exp)
  (expect-and (expect-pred procedure?)
              (expect-procedure-arity-includes? 0)
              exp))

(define (make-not-raise-fault proc)
  (with-handlers ([(const #t) raise-fault]) (proc) #f))

(define expect-not-raise (expect-thunk (expect/singular make-not-raise-fault)))

(module+ test
  (check-equal? (expectation-apply expect-not-raise void) (list))
  (check-equal? (expectation-apply expect-not-raise identity)
                (list (fault #:summary "a procedure accepting no arguments"
                             #:expected (arity-includes-attribute 0)
                             #:actual (arity-attribute identity))))
  (check-equal? (expectation-apply expect-not-raise (thunk (raise 'foo)))
                (list (fault #:summary "no value raised during procedure call"
                             #:expected the-not-raise-attribute
                             #:actual (raise-attribute 'foo)))))

(struct raise-context context ()
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-raise-context)

(define (raise-context) (make-raise-context "the raised value"))

(struct raise-any-attribute attribute ()
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-raise-any-attribute)

(define raise-any-attribute (make-raise-any-attribute "raised a value"))

(define (expect-raise exp)
  (expect-thunk
   (expectation
    (λ (proc)
      (define exp/context (expect/context exp (raise-context)))
      (with-handlers ([(const #t) (expectation-apply exp/context _)])
        (proc)
        (list (fault #:summary "a value raised during procedure call"
                     #:expected raise-any-attribute
                     #:actual the-not-raise-attribute)))))))

(module+ test
  (check-equal? (expectation-apply (expect-raise (expect-equal? 'foo))
                                          (thunk (raise 'foo)))
                (list))
  (define raise-proc (thunk (raise 'bar)))
  (check-equal? (expectation-apply (expect-raise (expect-equal? 'foo))
                                          raise-proc)
                (list (fault #:summary "a different value"
                             #:expected (equal-attribute 'foo)
                             #:actual (self-attribute 'bar)
                             #:contexts (list (raise-context)))))
  (check-equal? (expectation-apply (expect-raise (expect-equal? 'foo)) void)
                (list (fault #:summary "a value raised during procedure call"
                             #:expected raise-any-attribute
                             #:actual the-not-raise-attribute))))

(struct return-context context ()
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-return-context)

(define return-context (make-return-context "return value"))

(define (expect-return exp)
  (define exp/context (expect/context exp return-context))
  (expect-thunk
   (expectation
    (λ (proc)
      (with-handlers ([(const #t) (λ (e) (list (raise-fault e)))])
        (expectation-apply exp/context (proc)))))))

(module+ test
  (define exp-return-foo (expect-return (expect-equal? 'foo)))
  (check-equal? (expectation-apply exp-return-foo (thunk 'foo)) (list))
  (define bar-thunk (thunk 'bar))
  (check-equal? (expectation-apply exp-return-foo bar-thunk)
                (list (fault #:summary "a different value"
                             #:expected (equal-attribute 'foo)
                             #:actual (self-attribute 'bar)
                             #:contexts (list return-context))))
  (define raise-thunk (thunk (raise 'error)))
  (check-equal? (expectation-apply exp-return-foo raise-thunk)
                (list (fault #:summary "no value raised during procedure call"
                             #:expected the-not-raise-attribute
                             #:actual (raise-attribute 'error)))))

(struct call-context context (args)
  #:transparent #:omit-define-syntaxes #:constructor-name make-call-context)

(define (call-context args)
  (make-call-context (format "call with ~v" args) args))

(define (apply/arguments f args)
  (define kw+vs (hash->list (arguments-keyword args)))
  (keyword-apply f (map car kw+vs) (map cdr kw+vs) (arguments-positional args)))

(module+ test
  (define (add/kw a b #:kw c) (+ a b c))
  (check-equal? (apply/arguments add/kw (arguments 1 2 #:kw 3)) 6))

(define (expect-call args call-exp)
  (define num-positional (length (arguments-positional args)))
  (define has-kws? (not (hash-empty? (arguments-keyword args))))
  (expect-and (expect-pred procedure?)
              (expect-procedure-arity-includes? num-positional
                                                #:keywords-okay? has-kws?)
              (expectation
               (λ (proc)
                 (define (call) (apply/arguments proc args))
                 (define call-exp/context
                   (expect/context call-exp (call-context args)))
                 (expectation-apply call-exp/context call)))))

(module+ test
  (define exp-add
    (expect-call (arguments 1 2) (expect-return (expect-equal? 3))))
  (check-equal? (expectation-apply exp-add +) (list)))
