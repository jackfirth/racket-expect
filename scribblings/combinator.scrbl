#lang scribble/manual

@(require "base.rkt")

@title{Expectation Combinators}

Due to the structured representation of @fault-tech{faults},
@expectation-tech{expectations} can be composed and extended in a variety of
ways. Several combinators are provided to make it easier to construct complex
expectations out of simple ones while preserving error message quality.

@section{Logical Combinators}

@defproc[(expect-all [exp expectation?] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects everything that each of
 the given @racket[exp] values expects. All expectations are tested and each
 expectation's @fault-tech{faults} are returned together with all other faults.
 To combine expectations in a way where later expectations are tested only if
 earlier ones pass, see @racket[expect-and].
 @(expect-examples
   (define positive-even
     (expect-all (expect-pred positive?) (expect-pred even?)))
   (expect! positive-even 4)
   (eval:error (expect! positive-even 7))
   (eval:error (expect! positive-even -4))
   (eval:error (expect! positive-even -7)))}

@defproc[(expect-and [exp expectation?] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects everything that each of
 the given @racket[exp] values expects. Each expectation is tested in order and
 if any expectation finds any @fault-tech{faults}, those faults are returned
 immediately and the remaining @racket[exp] expectations are not tested. To
 combine expectations in a way where all expectations are tested and all faults
 returned together, see @racket[expect-all].
 @(expect-examples
   (define (small? x) (< x 10))
   (define small-number
     (expect-and (expect-pred real?)
                 (expect-pred positive?)
                 (expect-pred small?)))
   (expect! small-number 5)
   (eval:error (expect! small-number 20))
   (eval:error (expect! small-number -4))
   (eval:error (expect! small-number 'foo)))}

@defproc[(expect/context [exp expectation?] [ctxt context?]) expectation?]{
 Returns an @expectation-tech{expectation} that behaves the same as @racket[exp]
 except that any @fault-tech{faults} returned have @racket[ctxt] as an
 additional @context-tech{context}. The extra context is added to the beginning
 of each fault's list of contexts, not the end.
 @(expect-examples
   (struct test-context context () #:transparent)
   (define test-exp (expect/context (expect-eq? 'foo) (test-context "test")))
   (eval:error (expect! test-exp 5)))}

@defproc[(expect/proc [exp expectation?] [proc (-> any/c any/c)]) expectation?]{
 Returns an @expectation-tech{expectation} that behaves like @racket[exp] except
 that input values are passed to @racket[proc] and the result is given to
 @racket[exp]. Consider combining the expectation with @racket[expect-pred] and
 @racket[expect-and] to ensure the input value is not given to @racket[proc]
 when it would break any contracts on @racket[proc].
 @(expect-examples
   (define first-foo (expect/proc (expect-eq? 'foo) first))
   (expect! first-foo '(foo bar))
   (eval:error (expect! first-foo '(bar foo))))}

@section{Compound Data Structure Combinators}

@defproc[(expect-list [item-exp expectation?] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a list whose
 elements satisfy the @racket[item-exp] expectations. The length of the list is
 also checked, and only the @racket[item-exp] expectations for lists that
 contain enough items to include the corresponding @racket[item-exp] are
 checked.
 @(expect-examples
   (define num+string-expectation
     (expect-list (expect-pred number?) (expect-pred string?)))
   (expect! num+string-expectation '(10 "text"))
   (eval:error (expect! num+string-expectation '(foo bar)))
   (eval:error (expect! num+string-expectation '(foo))))}
