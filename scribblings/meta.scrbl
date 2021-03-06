#lang scribble/manual

@(require "base.rkt")

@title{Meta Expectations}

These functions construct @expectation-tech{expectations} for asserting
properties of other expectations. This is especially useful when testing custom
expectations.

@defproc[(expect-exp-faults [input any/c]
                            [fault-exp (or/c expectation? fault?)] ...)
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value that is itself
 an expectation. That expectation is applied to @racket[input] and is expected
 to return one @fault-tech{fault} for each @racket[fault-exp]. Each returned
 fault is checked against the corresponding @racket[exp]. If any @racket[exp] is
 not an expectation, it is converted to one with @racket[->expectation]. See
 also @racket[expect-exp-faults*].

 @(expect-examples
   (expect! expect-true (expect-exp-faults #f expect-any))
   (eval:error (expect! expect-true (expect-exp-faults #f))))}

@defproc[(expect-exp-faults* [input any/c]
                             [fault-exp* (or/c expectation?
                                               (listof (or/c fault?
                                                             expectation?)))])
         expectation?]{
 Like @racket[expect-exp-faults], but the entire list of faults returned by
 applying a subject expectation to @racket[input] is checked against
 @racket[fault-exp*].

 @(expect-examples
   (define (expect-exp-even-faults input)
     (expect-exp-faults* input (expect-list-length (expect-pred even?))))
   (define exp-ab (expect-list 1 2))
   (expect! exp-ab (expect-exp-even-faults '(1 2)))
   (expect! exp-ab (expect-exp-even-faults '(a b)))
   (eval:error (expect! exp-ab (expect-exp-even-faults '(1 foo)))))}

@defproc[(expect-exp-apply [v any/c] [thunk-exp expectation?]) expectation?]{
 A more general form of @racket[expect-exp-faults] and
 @racket[expect-exp-faults*]. Returns an @expectation-tech{expectation} that
 expects a value @var[e] that is itself an expectation. Then, a thunk wrapping
 the expression @racket[(expectation-apply e v)] is created and checked against
 @racket[thunk-exp]. For @racket[thunk-exp], using @racket[expect-return] checks
 the returned faults of the expectation just like @racket[expect-exp-faults] and
 @racket[expect-exp-faults*].

 @(expect-examples
   (expect! expect-any (expect-exp-apply 'foo (expect-return '())))
   (define error-exp (expectation (λ (_) (raise 'kaboom!)) #:name 'error))
   (expect! error-exp (expect-exp-apply 'foo (expect-raise 'kaboom!)))
   (eval:error
    (expect! error-exp (expect-exp-apply 'foo (expect-return '())))))}

@defproc[(expect-fault
          [#:summary summary-exp any/c expect-any]
          [#:actual actual-exp any/c expect-any]
          [#:expected expected-exp any/c expect-any]
          [#:contexts contexts-exp any/c expect-any])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a @fault-tech{fault}
 whose summary, actual, expected, and contexts fields are then checked against
 @racket[summary-exp], @racket[actual-exp], @racket[expected-exp], and
 @racket[contexts-exp] respectively.

 @(expect-examples
   (define flt
     (fault #:summary "test fault"
            #:expected (make-self-attribute 'foo)
            #:actual (make-self-attribute 'bar)))
   (expect! flt (expect-fault))
   (expect! flt (expect-fault #:actual (make-self-attribute 'bar)))
   (eval:error (expect! flt (expect-fault #:summary "not test fault"))))}

@deftogether[
 (@defstruct*[(expect-context context) ([input any/c])
              #:transparent #:omit-constructor]
   @defproc[(make-expect-context [input any/c]) expect-context?])]{
 A @context-tech{context} and its constructor that represents the thunk created
 by wrapping a call to @racket[expectation-apply] with the subject expectation
 and @racket[input]. Used by @racket[expect-exp-apply] and its derivatives.}
