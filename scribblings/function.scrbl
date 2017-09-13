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
     (expect-return* (expect-list-count (expect-pred even?))))
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

@section{Procedure Context Structures}

@deftogether[
 (@defstruct*[(return-context context) () #:transparent #:omit-constructor]
   @defthing[the-return-context return-context?])]{
 A @context-tech{context} that represents the list of return values in a
 procedure call.}

@deftogether[
 (@defstruct*[(raise-context context) () #:transparent #:omit-constructor]
   @defthing[the-raise-context raise-context?])]{
 A @context-tech{context} that represents the value that was given to
 @racket[raise] in a procedure call that aborted.}

@deftogether[
 (@defstruct*[(call-context context) ([args arguments?])
              #:transparent #:omit-constructor]
   @defproc[(make-call-context [args arguments?]) call-context?])]{
 A @context-tech{context} and its constructor that represents a call to a
 procedure with @racket[args] passed as the procedure's arguments.}

@deftogether[
 (@defstruct*[(arity-context context) () #:transparent #:omit-constructor]
   @defthing[the-arity-context arity-context?])]{
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
