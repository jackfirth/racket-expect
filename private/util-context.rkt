#lang racket/base

(provide define-singleton-contexts)

(require syntax/parse/define
         "base.rkt")


(define-simple-macro
  (define-singleton-context id:id format:str)
  (begin
    (struct id context ()
      #:transparent #:constructor-name make #:omit-define-syntaxes)
    (define id (make format))))

(define-simple-macro (define-singleton-contexts (~seq id:id format:str) ...+)
  (begin (define-singleton-context id format) ...))

