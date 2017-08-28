#lang scribble/manual

@(require "base.rkt")

@title{Meta Expectations}

These functions construct @expectation-tech{expectations} for asserting
properties of other expectations. This is especially useful when testing custom
expectations.

@defproc[(expect-exp-faults [v any/c] [exp expectation-convertible?])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value that is itself
 an expectation. Additionally, that expectation is applied to @racket[v] and the
 list of @fault-tech{faults} it returns is checked against @racket[exp].

 @(expect-examples
   (expect! expect-true (expect-exp-faults #f (expect-list-count 1))))}

@defproc[(expect-exp-no-faults [v any/c]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value that is itself
 an expectation. Additionally, that expectation must not return any
 @fault-tech{faults} when applied to @racket[v].

 @(expect-examples
   (expect! expect-true (expect-exp-no-faults #t))
   (eval:error (expect! expect-true (expect-exp-no-faults #f))))}

@defproc[(expect-exp-one-fault [v any/c]
                               [exp expectation-convertible? expect-any])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value that is itself
 an expectation. Additionally, that expectation is applied to @racket[v] and it
 must return one @fault-tech{fault} which is then checked against @racket[exp].

 @(expect-examples
   (expect! expect-true (expect-exp-one-fault #f))
   (eval:error (expect! expect-true (expect-exp-one-fault #t))))}

@defproc[(expect-fault
          [#:summary summary-exp expectation-convertible? expect-any]
          [#:actual actual-exp expectation-convertible? expect-any]
          [#:expected expected-exp expectation-convertible? expect-any]
          [#:contexts contexts-exp expectation-convertible? expect-any])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a @fault-tech{fault}
 whose summary, actual, expected, and contexts fields are then checked against
 @racket[summary-exp], @racket[actual-exp], @racket[expected-exp], and
 @racket[contexts-exp] respectively.

 @(expect-examples
   (define flt
     (fault #:summary "test fault"
            #:expected (self-attribute 'foo)
            #:actual (self-attribute 'bar)))
   (expect! flt (expect-fault))
   (expect! flt (expect-fault #:actual (expect-equal? (self-attribute 'bar))))
   (eval:error (expect! flt (expect-fault #:summary "not test fault"))))}

@defproc[(expect-attribute [attr-exp expectation-convertible? expect-any])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects an
 @attribute-tech{attribute} whose description is then checked against
 @racket[expect-any].

 @(expect-examples
   (struct test-attribute attribute () #:transparent)
   (define attr (test-attribute "some description"))
   (expect! attr (expect-attribute))
   (expect! attr (expect-attribute "some description"))
   (eval:error (expect! attr (expect-attribute "some other description"))))}
