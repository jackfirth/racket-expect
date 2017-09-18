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
 @racket[args]. See also @racket[expect-call-exn].
 @(expect-examples
   (define exp-addition (expect-call (arguments 3 8) (expect-return 11)))
   (expect! + exp-addition)
   (eval:error (expect! - exp-addition))
   (eval:error (expect! (thunk 'wrong-arity) exp-addition))
   (eval:error (expect! (thunk* (raise 'error)) exp-addition)))}

@defproc[(expect-apply [f procedure?] [call-exp expectation?]) expectation?]{
 The inverse of @racket[expect-call]. Returns an @expectation-tech{expectation}
 that expects an @racket[arguments] value and checks @racket[call-exp] on a
 thunk wrapping a call to @racket[f] with the arguments. Like
 @racket[expect-call], to check the return value or raised values use
 @racket[expect-return], @racket[expect-raise], or @racket[expect-not-raise] for
 @racket[call-exp]. See also @racket[expect-apply-exn].
 @(expect-examples
   (define exp-add1=10 (expect-apply add1 (expect-return 10)))
   (expect! (arguments 9) exp-add1=10)
   (eval:error (expect! (arguments 2) exp-add1=10)))}

@defproc[(expect-return [value-exp any/c] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a thunk that returns one
 value for each @racket[value-exp]. Then, each returned value is checked against
 the corresponding @racket[value-exp]. Each @racket[value-exp] is converted to
 an expectation with @racket[->expectation]. To assert properties about the list
 of values as a whole, see @racket[expect-return*].
 @(expect-examples
   (expect! (thunk 'foo) (expect-return 'foo))
   (eval:error (expect! (thunk 'bar) (expect-return 'foo)))
   (eval:error (expect! (thunk (raise 'error)) (expect-return 'foo)))
   (expect! (thunk (values 'foo 'bar)) (expect-return 'foo 'bar)))}

@defproc[(expect-return* [values-exp (or/c list? expectation?)]) expectation?]{
 Like @racket[expect-return], but returns an @expectation-tech{expectation} that
 expects a thunk, then calls that thunk and checks the list of values returned
 against @racket[values-exp]. If @racket[values-exp] is a list, it is converted
 to an expectation with @racket[->expectation].
 @(expect-examples
   (define expect-even-values
     (expect-return* (expect-list-length (expect-pred even?))))
   (expect! (thunk (values)) expect-even-values)
   (expect! (thunk (values 'foo 'bar)) expect-even-values)
   (eval:error (expect! (thunk 'foo) expect-even-values)))}

@defproc[(expect-raise [raise-exp any/c]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a thunk @racket[raise]s
 a value which is then checked against @racket[raise-exp]. The given
 @racket[raise-exp] is converted to an expectation with @racket[->expectation].
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

@defproc[(expect-exn [msg-exp (or/c string? regexp? expectation?) expect-any])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects an @racket[exn] value or
 a subtype. The input exception's message is then checked against
 @racket[msg-exp]. If @racket[msg-exp] is a regexp, it is converted to an
 expectation with @racket[(expect-regexp-match msg-exp)]; otherwise it is
 converted with @racket[->expectation]. See also @racket[expect-call-exn] and
 @racket[expect-apply-exn].

 @(expect-examples
   (define foo-exn (make-exn "foo exception" (current-continuation-marks)))
   (expect! foo-exn (expect-exn #rx"foo"))
   (expect! foo-exn (expect-exn "foo exception"))
   (eval:error (expect! foo-exn (expect-exn "foo")))
   (eval:error (expect! 'not-an-exn (expect-exn))))}

@defproc[(expect-call-exn
          [args arguments?]
          [msg-exp (or/c string? regexp? expectation?) expect-any])
         expectation?]{
 Convenient shorthand for
 @racket[(expect-call args (expect-raise (expect-exn msg-exp)))].}

@defproc[(expect-apply-exn
          [f procedure?]
          [msg-exp (or/c string? regexp? expectation?) expect-any])
         expectation?]{
 Convenient shorthand for
 @racket[(expect-apply args (expect-raise (expect-exn msg-exp)))].}

@section{Procedure Context Structures}

@defthing[the-return-context context?]{
 A @context-tech{context} that represents the list of return values in a
 procedure call.}

@defthing[the-raise-context context?]{
 A @context-tech{context} that represents the value that was given to
 @racket[raise] in a procedure call that aborted with an exception.}

@deftogether[
 (@defstruct*[(call-context context) ([args arguments?])
              #:transparent #:omit-constructor]
   @defproc[(make-call-context [args arguments?]) call-context?])]{
 A @context-tech{context} and its constructor that represents the thunk created
 by calling the subject procedure with @racket[args].}

@deftogether[
 (@defstruct*[(apply-context context) ([proc procedure?])
              #:transparent #:omit-constructor]
   @defproc[(make-apply-context [proc procedure?]) apply-context?])]{
 A @context-tech{context} and its constructor that represents the thunk created
 by applying the subject arguments to @racket[proc].}

@defthing[the-arity-context context?]{
 A @context-tech{context} that represents the arity of a procedure, as returned
 by @racket[procedure-arity].}

@section{Procedure Attribute Structures}

@deftogether[
 (@defstruct*[(arity-includes-attribute attribute) ([value procedure-arity?])
              #:transparent #:omit-constructor]
   @defproc[(make-arity-includes-attribute [arity procedure-arity?])
            arity-includes-attribute?])]{
 An @attribute-tech{attribute} and its constructor that represents an arity that
 a procedure includes, in the sense of @racket[arity-includes?]. This is
 distinct from @racket[arity-attribute] in that the procedure's actual arity may
 not be @racket[arity=?] to the @racket[value] arity.}
