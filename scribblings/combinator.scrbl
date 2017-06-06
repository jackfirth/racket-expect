#lang scribble/manual

@(require "base.rkt")

@title{Expectation Combinators}

Due to the structured representation of @fault-tech{faults},
@expectation-tech{expectations} can be composed and extended in a variety of
ways. Several combinators are provided to make it easier to construct complex
expectations out of simple ones while preserving error message quality.

@defproc[(expect/context [exp expectation?] [ctxt context?]) expectation?]{
 Returns an @expectation-tech{expectation} that behaves the same as @racket[exp]
 except that any @fault-tech{faults} returned have @racket[ctxt] as an
 additional @context-tech{context}. The extra context is added to the beginning
 of each fault's list of contexts, not the end.
 @(expect-examples
   (struct test-context context () #:transparent)
   (define test-exp (expect/context (expect-eq? 'foo) (test-context "test")))
   (eval:error (expect! 5 test-exp)))}

@defproc[(expect/proc [exp expectation?] [proc (-> any/c any/c)]) expectation?]{
 Returns an @expectation-tech{expectation} that behaves like @racket[exp] except
 that input values are passed to @racket[proc] and the result is given to
 @racket[exp]. Consider combining the expectation with @racket[expect-pred] and
 @racket[expect-and] to ensure the input value is not given to @racket[proc]
 when it would break any contracts on @racket[proc].
 @(expect-examples
   (define first-foo (expect/proc (expect-eq? 'foo) first))
   (expect! '(foo bar) first-foo)
   (eval:error (expect! '(bar foo) first-foo)))}
