#lang scribble/manual

@(require "base.rkt")

@title{Procedure Expectations}

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
