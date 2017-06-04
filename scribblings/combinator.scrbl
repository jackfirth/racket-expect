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
   (expect! 4 positive-even)
   (eval:error (expect! 7 positive-even))
   (eval:error (expect! -4 positive-even))
   (eval:error (expect! -7 positive-even)))}

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
   (expect! 5 small-number)
   (eval:error (expect! 20 small-number))
   (eval:error (expect! -4 small-number))
   (eval:error (expect! 'foo small-number)))}

@section{Extending Expectations}

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

@section{Compound Data Structure Combinators}

@defproc[(expect-list [item-exp expectation-convertible?] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a list whose
 elements satisfy the @racket[item-exp] expectations. The length of the list is
 also checked, and only the @racket[item-exp] expectations for lists that
 contain enough items to include the corresponding @racket[item-exp] are
 checked.
 @(expect-examples
   (define num+string-expectation
     (expect-list (expect-pred number?) (expect-pred string?)))
   (expect! '(10 "text") num+string-expectation)
   (eval:error (expect! '(foo bar) num+string-expectation))
   (eval:error (expect! '(foo) num+string-expectation)))}

@defproc[(expect-vector [item-exp expectation-convertible?] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a vector
 whose elements satisfy the @racket[item-exp] expectations. The length of the
 vector is also checked, and only the @racket[item-exp] expectations for vectors
 that contain enough items to include the corresponding @racket[item-exp] are
 checked.
 @(expect-examples
   (define num+foo-vec-expectation (expect-vector (expect-pred number?) 'foo))
   (expect! #(10 foo) num+foo-vec-expectation)
   (eval:error (expect! #(10 bar) num+foo-vec-expectation))
   (eval:error (expect! #(10) num+foo-vec-expectation)))}

@section{Procedure Expectations}

@defproc[(expect-call [args arguments?] [call-exp expectation?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a procedure and checks
 @racket[call-exp] on a thunk wrapping a call to that procedure with
 @racket[args]. Use with @racket[expect-return] to check the return value of the
 procedure call and with @racket[expect-raise] or @racket[expect-not-raise] to
 check how the procedure call behaves with respect to raised errors. The
 expected procedure's arity is checked to ensure it can be called with
 @racket[args].
 @(expect-examples
   (define exp-addition (expect-call (arguments 3 8) (expect-return 11)))
   (expect! + exp-addition)
   (eval:error (expect! - exp-addition))
   (eval:error (expect! (thunk 'wrong-arity) exp-addition))
   (eval:error (expect! (thunk* (raise 'error)) exp-addition)))}

@defproc[(expect-return [value-exp expectation-convertible?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a thunk returns a value,
 then that value is checked against @racket[value-exp].
 @(expect-examples
   (expect! (thunk 'foo) (expect-return 'foo))
   (eval:error (expect! (thunk 'bar) (expect-return 'foo)))
   (eval:error (expect! (thunk (raise 'error)) (expect-return 'foo))))}

@defproc[(expect-raise [raise-exp expectation-convertible?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a thunk @racket[raise]s
 a value which is then checked against @racket[raise-exp].
 @(expect-examples
   (define (raise-foo) (raise 'foo))
   (expect! raise-foo (expect-raise 'foo))
   (define (success) 'success)
   (eval:error (expect! success (expect-raise 'foo)))
   (define (raise-bar) (raise 'bar))
   (eval:error (expect! raise-bar (expect-raise 'foo))))}

@defthing[expect-not-raise expectation?]{
 An expectation that expects a thunk does not @racket[raise] any value when
 called.
 @(expect-examples
   (expect! (thunk 'success) expect-not-raise)
   (eval:error (expect! (thunk (raise 'failure)) expect-not-raise))
   (define (not-a-thunk unexpected-arg)
     'foo)
   (eval:error (expect! not-a-thunk expect-not-raise)))}

@section{Conversion to Expectations}

@defproc[(expectation-convertible? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is a value that can be converted to an
 @expectation-tech{expectation}, returns @racket[#f] otherwise. Several kinds of
 values are convertible:

 @itemlist[
 @item{Any expectation (according to @racket[expectation?]) is convertible to
   itself.}
 @item{Booleans are convertible to either @racket[expect-true] or
   @racket[expect-false].}
 @item{Numbers, strings, symbols, and characters are convertible to expectations
   constructed with @racket[expect-equal?].}
 @item{Lists are convertible with @racket[expect-list] after first converting
   their contents.}
 @item{Vectors are convertible with @racket[expect-vector] after first
   converting their contents.}]}

@defproc[(expectation-convert [v expectation-convertible?]) expectation?]{
 Returns an @expectation-tech{expectation} constructed by converting @racket[v]
 to an expectation. See @racket[expectation-convertible?] for a description of
 what values are and aren't convertible.}
