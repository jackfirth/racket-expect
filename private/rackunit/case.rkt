#lang racket/base

(provide test-subject)

(require (for-syntax racket/base
                     syntax/parse)
         racket/function
         rackunit
         syntax/parse/define
         syntax/srcloc
         "base.rkt")


(begin-for-syntax
  (define-splicing-syntax-class test-name
    #:attributes ([test 1])
    (pattern (~seq name:str) #:attr [test 1] (list #'test-case #'name))
    (pattern (~seq) #:attr [test 1] (list #'test-begin))))

(define-check (check-subject sub exp exp-stx)
  (define infos
    (list (make-check-name 'test-expect)
          (make-check-params #f)
          (make-check-location (build-source-location-list exp-stx))
          (make-check-expression (syntax->datum exp-stx))))
  (with-check-info* infos
    (thunk (fail-check/expect sub exp))))

(define-simple-macro
  (test-subject name:test-name #:subject sub*:expr exp:expr ...+)
  (name.test ...
   (define sub sub*)
   (check-subject sub exp #'exp) ...))
