#lang racket/base

(provide (for-label (all-from-out arguments
                                  expect
                                  racket/base
                                  racket/contract
                                  racket/format
                                  racket/function
                                  racket/list
                                  racket/set))
         expect-examples
         source-code-link)

(require (for-label arguments
                    expect
                    racket/base
                    racket/contract
                    racket/format
                    racket/function
                    racket/list
                    racket/set)
         scribble/example
         scribble/manual
         scribble/text
         syntax/parse/define
         "util.rkt")


(define (source-code-link github-str)
  (begin/text "Source code for this library is avaible "
              (hyperlink github-str "on Github")))

(define (make-expect-eval)
  (make-base-eval #:lang 'racket/base
                  '(require arguments
                            expect
                            racket/function
                            racket/list
                            racket/set)))

(define-simple-macro (expect-examples example:expr ...)
  (examples #:eval (make-expect-eval) example ...))

(define-tech-helpers
  attribute-tech "fault-attribute"
  context-tech "fault-context"
  expectation-tech "expectation"
  fault-tech "expectation-fault")
