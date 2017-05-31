#lang racket/base

(provide rest->)

(require racket/contract)

(define (rest-> arg/c result/c) (->* () #:rest (listof arg/c) result/c))
