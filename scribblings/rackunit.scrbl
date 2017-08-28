#lang scribble/manual

@(require "base.rkt")

@title{Using Expectations with RackUnit}
@defmodule[expect/rackunit]

This module defines how to use @expectation-tech{expectations} with the
@racketmodname[rackunit] testing framework. Included is a custom check that
uses an expectation to test a value, as well as expectation-using replacements
for the various built in checks provided by @racketmodname[rackunit].

If you have an existing set of RackUnit tests, simply change
@racket[(require rackunit)] to @racket[(require expect/rackunit)]. Only checks
are exported by @racketmodname[expect/rackunit]; if your tests use other exports
of @racketmodname[rackunit] you'll need to use @racket[only-in] to import them
from RackUnit.

@defproc[(check-expect [v any/c] [exp expectation?] [message string? ""])
         void?]{
 Checks that @racket[v] has no @fault-tech{faults} according to @racket[exp],
 with @racket[message] added to the check info stack in the event of failure.

 @(expect-examples
   (check-expect 1 (expect-pred number?))
   (check-expect 'foo (expect-pred number?))
   (check-expect #hash((a . (1 WRONG 3)) (b . (4 5 WRONG)))
                 (expect-equal? #hash((a . (1 2 3)) (b . (4 5 6))))))}

@(define-simple-macro (defchecks check-id:id ...+ pre-flow:expr ...)
   (deftogether ((defthing check-id procedure?) ...) pre-flow ...))

@defchecks[check-eq? check-eqv? check-equal?
           check-not-eq? check-not-eqv? check-not-equal?
           check-pred check-=
           check-true check-false check-not-false
           check-exn check-not-exn
           check]{
 Custom checks that are equivalent to their corresponding
 @racketmodname[rackunit] checks except that they are defined in terms of
 @expectation-tech{expectations}, and provide better error messages for most
 failures.
 @(expect-examples
   (check-equal? '(1 2 3 foo 5) '(1 2 3 4 5)))}
