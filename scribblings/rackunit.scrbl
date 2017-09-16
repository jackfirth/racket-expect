#lang scribble/manual

@(require "base.rkt")

@title{Using Expectations with RackUnit}
@defmodule[expect/rackunit]

This module defines how to use @expectation-tech{expectations} with the
@racketmodname[rackunit] testing framework. Included are custom checks and test
forms that test values using expectations, as well as expectation-using
replacements for the various built in checks provided by
@racketmodname[rackunit].

If you have an existing set of RackUnit tests, simply change
@racket[(require rackunit)] to @racket[(require expect/rackunit)]. Only checks
are exported by @racketmodname[expect/rackunit]; if your tests use other exports
of @racketmodname[rackunit] you'll need to use @racket[only-in] to import them
from RackUnit.

@defproc[(check-expect [v any/c] [exp any/c] [message string? ""])
         void?]{
 Checks that @racket[v] has no @fault-tech{faults} according to @racket[exp],
 with @racket[message] added to the check info stack in the event of failure.
 The given @racket[exp] is converted to an expectation with
 @racket[->expectation].

 @(expect-examples
   (check-expect 1 (expect-pred number?))
   (check-expect 'foo (expect-pred number?))
   (check-expect #hash((a . (1 WRONG 3)) (b . (4 5 WRONG)))
                 #hash((a . (1 2 3)) (b . (4 5 6)))))}

@defchecks[check-eq? check-eqv? check-equal?
           check-not-eq? check-not-eqv? check-not-equal?
           check-pred check-=
           check-true check-false check-not-false
           check-exn check-not-exn
           check]{
 Custom checks that are equivalent to their corresponding
 @racketmodname[rackunit] checks except that they are defined in terms of
 @expectation-tech{expectations}, and provide better error messages for most
 failures. None of these checks accept expectations as arguments.
 @(expect-examples
   (check-equal? '(1 2 3 foo 5) '(1 2 3 4 5)))}

@defform[(test-subject maybe-name
           #:subject subject-expr
           expectation-expr ...)
         #:grammar ([maybe-name (code:line) name-str])
         #:contracts ([subject-expr any/c]
                      [expectation-expr expectation?])]{
 Creates a @racket[test-case] or @racket[test-begin] form containing one
 @racket[check-expect] expression for each @racket[expectation-expr]. The
 @racket[subject-expr] is evaluated once and the resulting value is passed as
 the value to test by each @racket[check-expect] expression. If
 @racket[name-str] is provided, it must be a string and the resulting test is a
 @racket[test-case] with @racket[name-str] as its name, otherwise it is a
 @racket[test-begin] form.

 @(expect-examples
   (test-subject "addition" #:subject +
     (expect-call (arguments 1 2 3) (expect-return 6))
     (expect-call (arguments) (expect-return 0))
     (expect-call-exn (arguments 'foo) #rx"contract")
     (expect-call (arguments 5 10) (expect-return 16))))}

@defproc[(fail-check/expect [v any/c] [exp any/c]) void?]{
 Essentially equivalent to @racket[(check-expect v exp)], except as an ordinary
 function that raises an @racket[exn:test:check] exception. Check infos
 containing the @fault-tech{faults} raised by @racket[exp] are added to the
 exception, but no other infos (such as @racket['expression]) are included.}
