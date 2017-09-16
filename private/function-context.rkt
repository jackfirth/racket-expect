#lang racket

(require racket/contract/base)

(provide
 (contract-out
  [struct (apply-context context)
    ([description string?] [proc procedure?]) #:omit-constructor]
  [make-apply-context (-> procedure? apply-context?)]
  [struct (return-context context)
    ([description string?]) #:omit-constructor]
  [the-return-context return-context?]))

(require expect/private/base)


(struct apply-context context (proc) #:transparent)
(define (make-apply-context proc)
  (apply-context (format "application to ~v" proc) proc))

(struct return-context context () #:transparent)
(define the-return-context (return-context "the return values list"))
