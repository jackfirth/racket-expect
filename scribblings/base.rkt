#lang racket/base

(provide (for-label (all-from-out arguments
                                  expect
                                  expect/rackunit
                                  racket/base
                                  racket/contract
                                  racket/format
                                  racket/function
                                  racket/list
                                  racket/set
                                  rackunit))
         (all-from-out syntax/parse/define)
         defchecks
         expect-examples
         source-code-link)

(require (for-label arguments
                    expect
                    expect/rackunit
                    racket/base
                    racket/contract
                    racket/format
                    racket/function
                    racket/list
                    racket/set
                    (except-in rackunit
                               check-eq?
                               check-eqv?
                               check-equal?
                               check-not-eq?
                               check-not-eqv?
                               check-not-equal?
                               check-pred
                               check-=
                               check-true
                               check-false
                               check-not-false
                               check-exn
                               check-not-exn
                               check))
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
                            expect/rackunit
                            racket/contract
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

(define-simple-macro (defchecks check-id:id ...+ pre-flow:expr ...)
  (deftogether ((defthing check-id procedure?) ...) pre-flow ...))
