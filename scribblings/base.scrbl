#lang scribble/manual

@(require "base.rkt")

@title{Data Model}

Conceptually, an @expectation-tech[#:definition? #t]{expectation} is a function
that returns a list of @fault-tech[#:definition? #t]{faults} about its input.
Faults are a difference between some expected @attribute-tech{attribute} of the
input and an actual attribute. Faults are often scoped to a
@context-tech{context}, which identifies where precisely in the input value the
discrepency exists. This section documents each of these structures and how to
use their basic functionalities.

@section{Expectations}

@defproc[(expectation? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is an @expectation-tech{expectation}, returns
 @racket[#f] otherwise.
 @(expect-examples
   (expectation? 6)
   (expectation? expect-true))}

@defproc[(expectation [proc (-> any/c (listof fault?))]) expectation?]{
 Returns an @expectation-tech{expectation} whose implementation is
 @racket[proc].
 @(expect-examples
   (define empty-expectation (expectation (λ (v) (list))))
   (expectation-apply empty-expectation 'foo))}

@defproc[(expectation-apply [exp expectation?] [v any/c]) result?]{
 Returns a @result-tech{result} that wraps @racket[v] with a list of faults
 found by @racket[exp]. See also @racket[expectation-apply/faults].
 @(expect-examples
   (expectation-apply (expect-equal? (list 1 2)) (list 1 2))
   (expectation-apply (expect-equal? (list 1 2)) (list 1 'foo)))}

@defproc[(expectation-apply/faults [exp expectation?] [v any/c])
         (listof faults?)]{
 Like @racket[expectation-apply], but returns only the list of faults found by
 @racket[exp] instead of wrapping the faults and @racket[v] in a
 @result-tech{result structure}. This is intended for when one expectation's
 implementation refers to other expectations, since expectation implementations
 don't return result structures directly.
 @(expect-examples
   (expectation-apply/faults (expect-equal? (list 1 2)) (list 1 2))
   (expectation-apply/faults (expect-equal? (list 1 2)) (list 1 'foo)))}

@defproc[(expect! [exp expectation?] [v any/c]) void?]{
 Checks that @racket[v] has no @fault-tech{faults} according to @racket[exp]. If
 it does, an instance of @racket[exn:fail:expect] is raised with a message
 detailing the faults.
 @(expect-examples
   (define list-exp (expect-equal? (list 1 2)))
   (expect! list-exp '(1 2))
   (eval:error (expect! list-exp '(1 a b))))}

@defstruct*[(exn:fail:expect exn:fail) ([result result?])]{
 An instance of @racket[exn:fail] that is thrown by @racket[expect!] when a
 value does not live up to an @expectation-tech{expectation}. The
 @racket[result] field includes both the original subject of the expectation and
 the list of @fault-tech{faults} found by the expectation.}

@section{Faults and Results}

@defproc[(fault? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a @fault-tech{fault}, returns @racket[#f]
 otherwise.
 @(expect-examples
   (fault? 6)
   (fault? (fault #:summary "test fault"
                  #:expected (self-attribute 'foo)
                  #:actual (self-attribute 'bar))))}

@defproc[(fault [#:summary summary string?]
                [#:expected expected attribute?]
                [#:actual actual attribute?]
                [#:contexts contexts (listof context?) (list)])
         fault?]{
 Returns a @fault-tech{fault} with the given summary, expected and actual
 attributes, and contexts. Contexts are given in order of least specific to most
 specific; see @racket[expect-list] for an example of proper use of contexts.
 @(expect-examples
   (fault #:summary "test fault"
          #:expected (self-attribute 'foo)
          #:actual (self-attribute 'bar))
   (struct test-context context () #:transparent)
   (fault #:summary "test fault with contexts"
          #:expected (self-attribute 'foo)
          #:actual (self-attribute 'bar)
          #:contexts (list (test-context "test context")
                           (test-context "nested test context"))))}

@deftogether[
 (@defproc[(fault-summary [flt fault?]) string?]
   @defproc[(fault-expected [flt fault?]) attribute?]
   @defproc[(fault-actual [flt fault?]) attribute?]
   @defproc[(fault-contexts [flt fault?]) (listof context?)])]{
 Accessors for the various fields of a @racket[fault?] structure. See
 @racket[fault] for information about these fields.}

@defproc[(result? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a @result-tech{result}, returns
 @racket[#f] otherwise.
 @(expect-examples
   (result? "asdf")
   (result? (expectation-apply expect-true 'foo)))}

@deftogether[
 (@defproc[(result-subject [rslt result?]) any/c]
   @defproc[(result-faults [rslt result?]) (listof fault?)])]{
 Accessors for fields of @result-tech[#:definition? #t]{result} values, which
 are returned by @racket[expectation-apply] to pair the original subject of an
 expectation's assertions with faults the expectation found.
 @(expect-examples
   (define rslt (expectation-apply expect-true 'foo))
   (result-subject rslt)
   (result-faults rslt))}

@section{Contexts and Attributes}

@defstruct*[context ([description string?]) #:transparent #:omit-constructor]{
 A structure type for what @context-tech[#:definition? #t]{context} a
 @fault-tech{fault} occurs in. Contexts are meant to be structured information,
 so that different clients can render descriptions of contexts in different
 ways. Every context includes a @racket[description] string so that a simple
 text description of the context can be constructed. The @racket[context]
 constructor is not provided; it's not possible to create a context without
 defining a subtype of the @racket[context] struct. Various expectations
 provided by this library define and provide their own context subtypes that add
 extra fields, see @racket[expect-list] for an example.}

@defstruct*[attribute ([description string?]) #:transparent #:omit-constructor]{
 A structure type for an @attribute-tech[#:definition? #t]{attribute}, some
 property of a value that a @fault-tech{fault} refers to. Like
 @context-tech{contexts}, attributes are structured information with a string
 description whose subtypes add additional fields for clients to use to
 construct error messages. Additionally, the @racket[attribute] constructor is
 not provided so the only way to create attributes is with a subtype. See
 @racket[self-attribute] for a trivial implementation.}

@defproc[(self-attribute? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is an @attribute-tech{attribute} returned by
 @racket[self-attribute], returns @racket[#f] otherwise.
 @(expect-examples
   (self-attribute? 'nope)
   (self-attribute? (self-attribute 'yup)))}

@defproc[(self-attribute [value any/c]) self-attribute?]{
 Constructs an @attribute-tech{attribute} whose description is
 @racket[(~v value)]. The @racket[value] used to construct the attribute can be
 retrieved later with @racket[self-attribute-value].
 @(expect-examples
   (self-attribute 'foo)
   (attribute-description (self-attribute 'foo)))}

@defproc[(self-attribute-value [self-attr self-attribute?]) any/c]{
 Returns the original value used to construct @racket[self-attr] with
 @racket[self-attribute].
 @(expect-examples
   (self-attribute-value (self-attribute 'foo)))}
