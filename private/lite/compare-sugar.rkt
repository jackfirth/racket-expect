#lang racket/base

;; This is only in a separate module so `raco cover` properly marks the macro
;; expansion as covered; a module boundary has to be crossed for that to happen.

(require (submod "compare.rkt" for-sugar))

(define-attr-sugar
  [make-eq-attribute eq-attribute? eq?]
  [make-eqv-attribute eqv-attribute? eqv?]
  [make-equal-attribute equal-attribute? equal?])
