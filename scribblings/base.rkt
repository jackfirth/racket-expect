#lang racket/base

(provide (for-label (all-from-out arguments
                                  expect
                                  racket/base
                                  racket/contract
                                  racket/format
                                  racket/function
                                  racket/list))
         expect-examples
         source-code-link)

(require (for-label arguments
                    expect
                    racket/base
                    racket/contract
                    racket/format
                    racket/function
                    racket/list)
         scribble/example
         scribble/manual
         scribble/text
         syntax/parse/define)

(define (source-code-link github-str)
  (begin/text "Source code for this library is avaible "
              (hyperlink github-str "on Github")))

(define (make-expect-eval)
  (make-base-eval #:lang 'racket/base
                  '(require arguments expect racket/function racket/list)))

(define-simple-macro (expect-examples example:expr ...)
  (examples #:eval (make-expect-eval) example ...))

(define ((tech-helper key) #:definition? [definition? #f] . pre-flow)
  (apply (if definition? deftech tech) #:key key pre-flow))

(define-simple-macro (define-tech-helpers (~seq id:id key:str) ...)
  (begin (begin (define id (tech-helper key)) (provide id)) ...))

(define-tech-helpers
  attribute-tech "fault-attribute"
  context-tech "fault-context"
  expectation-tech "expectation"
  fault-tech "expectation-fault"
  result-tech "expectation-result")
