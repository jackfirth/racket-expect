#lang racket/base

;; This would normally be a submodule of function.rkt, but it's not possible for
;; a module A to depend on another module B if B depends on a submodule of A,
;; even if that submodule does not depend on A (by using module instead of
;; module* or module+). This is in principle totally fine, but it hasn't been
;; implemented. See https://github.com/racket/racket/issues/1101.

(require racket/contract/base)

(provide
 (contract-out
  [expect-apply (-> procedure? expectation? expectation?)]
  [struct (apply-context context)
    ([description string?] [proc procedure?]) #:omit-constructor]
  [make-apply-context (-> procedure? apply-context?)]))

(require arguments
         "base.rkt"
         "combinator.rkt"
         "logic.rkt")


(define (expect-apply proc call-exp)
  (define call-exp* (expect/context call-exp (make-apply-context proc)))
  (define anon-exp
    (expect-and (expect-pred arguments?)
                (expectation
                 (λ (args)
                   (define (call) (apply/arguments proc args))
                   (expectation-apply call-exp* call)))))
  (expectation-rename anon-exp 'apply))

(struct apply-context context (proc) #:transparent)
(define (make-apply-context proc)
  (apply-context (format "application to ~v" proc) proc))
