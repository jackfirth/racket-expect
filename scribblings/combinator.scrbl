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
 @racket[exp].
 @(expect-examples
   (define first-foo (expect/proc (expect-eq? 'foo) first))
   (expect! '(foo bar) first-foo)
   (eval:error (expect! '(bar foo) first-foo)))}

@defproc[(expect/dependent [exp-proc (-> any/c expectation?)]) expectation?]{
 Returns an @expectation-tech{expectation} that passes its value to
 @racket[exp-proc] then checks the value against the expectation returned by
 @racket[exp-proc]. This is useful when the exact set of faults that a value
 could have depends on the shape of the value, such as in the case of
 @racket[expect-list] (which uses @racket[expect/dependent] under the hood).
 @(expect-examples
   (define (last-string-expectation vs)
     (expect-list-ref (expect-pred string?) (sub1 (length vs))))
   (define expect-last-string (expect/dependent last-string-expectation))
   (expect! '(a b "foo") expect-last-string)
   (eval:error (expect! '(a b c) expect-last-string)))}

@defproc[(expect/singular [fault-proc (-> any/c (or/c fault? #f))])
         expectation?]{
 Returns an @expectation-tech{expectation} that passes its value to
 @racket[fault-proc] and either returns the @fault-tech{fault} returned by
 @racket[fault-proc] or returns an empty list of faults if @racket[fault-proc]
 returns @racket[#f]. This is useful when an expectation could logically only
 return a single fault at most, removing the boilerplate of returning either a
 singular list or an empty list.
 @(expect-examples
   (struct single-digit-attribute attribute () #:transparent)
   (define (single-digit-fault v)
     (and (not (<= 0 v 9))
          (fault #:summary "a single digit positive integer"
                 #:expected (single-digit-attribute "integer between 0 and 9")
                 #:actual (make-self-attribute v))))
   (define expect-single-digit (expect/singular single-digit-fault))
   (expect! 5 expect-single-digit)
   (eval:error (expect! 123 expect-single-digit)))}
